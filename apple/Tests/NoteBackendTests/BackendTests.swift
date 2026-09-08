import Foundation
import XCTest
@testable import NoteBackend

final class BackendTests: XCTestCase {
    func testRoundTripAndErrorRecovery() async throws {
        try await MainActor.run {
            let directory = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
            let backend = Backend()
            defer {
                backend.close()
                try? FileManager.default.removeItem(at: directory)
            }
            let path = directory.appendingPathComponent("test.foonote").path
            let root = try backend.open(path)
            let first = try backend.insert(parent: root, text: "中文 📝\nSecond line\u{0}tail")
            let second = try backend.insert(parent: root, text: "")
            let child = try backend.insert(parent: first, text: "Child")
            XCTAssertEqual(try backend.children(root).suffix(2), [first, second])
            XCTAssertEqual(try backend.children(first), [child])
            XCTAssertEqual(try backend.text(first), "中文 📝\nSecond line\u{0}tail")
            XCTAssertEqual(try backend.text(second), "")
            try backend.setText(child, text: "Edited café 日本語")
            XCTAssertThrowsError(try backend.open("invalid-url"))
            XCTAssertEqual(try backend.text(child), "Edited café 日本語")
            try backend.persist()
            backend.close()
            XCTAssertThrowsError(try backend.text(first))
            let reopened = try backend.open(path)
            let children = try backend.children(reopened)
            let restoredFirst = children[children.count - 2]
            XCTAssertEqual(try backend.text(restoredFirst), "中文 📝\nSecond line\u{0}tail")
            let restoredChild = try XCTUnwrap(backend.children(restoredFirst).first)
            XCTAssertEqual(try backend.text(restoredChild), "Edited café 日本語")
            try backend.remove(restoredChild)
            XCTAssertTrue(try backend.children(restoredFirst).isEmpty)
            try backend.persist()
            backend.close()
            let finalRoot = try backend.open(path)
            let finalChildren = try backend.children(finalRoot)
            let finalFirst = finalChildren[finalChildren.count - 2]
            XCTAssertTrue(try backend.children(finalFirst).isEmpty)
        }
    }

    @MainActor
    func testHierarchyMetadataBatchMoveAndSearch() async throws {
            let backend = Backend()
            defer { backend.close() }
            let root = try backend.open("memory:")
            let a = try backend.insert(parent: root, text: "alpha", meta: "type=folder\n", position: 0)
            let b = try backend.insert(parent: root, text: "needle beta", position: 0)
            let c = try backend.insert(parent: root, text: "gamma", position: 0)
            let child = try backend.insert(parent: a, text: "child", position: 0)

            XCTAssertEqual(try backend.metadata(a), "type=folder\n")
            XCTAssertEqual(try backend.parent(child), a)

            // Move a selection into a folder, then place it after a sibling.
            _ = try backend.move([b], destination: a, position: 0)
            XCTAssertEqual(try backend.parent(b), a)
            _ = try backend.move([b], destination: c, position: 1)
            XCTAssertEqual(try backend.parent(b), root)
            XCTAssertEqual(try backend.children(root), [a, c, b])

            // The Rust backend normalizes ancestor/descendant selections.
            _ = try backend.move([a, child], destination: root, position: 0)
            XCTAssertEqual(try backend.children(root), [c, b, a])

            // Moving a node below its own descendant is rejected before mutation.
            XCTAssertThrowsError(try backend.move([a], destination: child, position: 0))
            XCTAssertEqual(try backend.parent(a), root)
            XCTAssertEqual(try backend.parent(child), a)
            // Validate all heads up front so a later cyclic head cannot leave an earlier one moved.
            XCTAssertThrowsError(try backend.move([c, a], destination: child, position: 0))
            XCTAssertEqual(try backend.parent(c), root)

            let before = try backend.insert(parent: c, text: "before", position: -1)
            XCTAssertEqual(try backend.children(root), [before, c, b, a])

            try backend.searchStart("needle", roots: [root])
            var results: [(NodeID, String)] = []
            for _ in 0..<100 where !(try backend.searchComplete()) {
                results = try backend.searchResults()
                try await Task.sleep(for: .milliseconds(5))
            }
            results = try backend.searchResults()
            XCTAssertTrue(try backend.searchComplete())
            XCTAssertTrue(results.contains { $0.0 == b && $0.1 == "needle beta" })
            try backend.searchStop()

            try backend.remove([child, a])
            XCTAssertEqual(try backend.children(root), [before, c, b])
    }

    func testLocalGitRootAndMountedGitPersistence() async throws {
        try await MainActor.run {
            let base = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
            try FileManager.default.createDirectory(at: base, withIntermediateDirectories: true)
            let gitURL = base.appendingPathComponent("notes.git")
            let localURL = base.appendingPathComponent("root.foonote")
            defer { try? FileManager.default.removeItem(at: base) }

            // A path ending in .git is a local Git backend; no remote is contacted.
            let git = Backend()
            let gitRoot = try git.open(gitURL.path)
            _ = try git.insert(parent: gitRoot, text: "stored in local git")
            try git.persist()
            git.close()

            let reopenedGit = Backend()
            let persistedRoot = try reopenedGit.open(gitURL.path)
            let gitChildren = try reopenedGit.children(persistedRoot)
            XCTAssertTrue(gitChildren.contains { id in
                (try? reopenedGit.text(id)) == "stored in local git"
            })
            reopenedGit.close()

            // A regular local notebook can mount that same Git repository through metadata.
            let rootBackend = Backend()
            let root = try rootBackend.open(localURL.path)
            let mount = try rootBackend.insert(
                parent: root,
                text: "Git mount",
                meta: "type=mount\nmount=\(gitURL.path)\n"
            )
            try rootBackend.persist()
            let mountedChildren = try rootBackend.children(mount)
            XCTAssertFalse(mountedChildren.isEmpty)
            XCTAssertEqual(try rootBackend.text(mountedChildren[0]), "stored in local git")
            XCTAssertNotEqual(mountedChildren[0].backend, mount.backend)
            let localNote = try rootBackend.insert(parent: root, text: "moved into git")
            let moved = try rootBackend.move([localNote], destination: mount, position: 0)
            let movedID = try XCTUnwrap(moved.first)
            XCTAssertNotEqual(movedID.backend, localNote.backend)
            XCTAssertEqual(try rootBackend.text(movedID), "moved into git")
            try rootBackend.setText(movedID, text: "edited through mount")
            try rootBackend.persist()
            rootBackend.close()

            let reopenedRootBackend = Backend()
            let reopenedRoot = try reopenedRootBackend.open(localURL.path)
            let reopenedMount = try reopenedRootBackend.children(reopenedRoot).first {
                (try? reopenedRootBackend.text($0)) == "Git mount"
            }
            let reopenedMountID = try XCTUnwrap(reopenedMount)
            let reopenedMountedChildren = try reopenedRootBackend.children(reopenedMountID)
            XCTAssertTrue(reopenedMountedChildren.contains { id in
                (try? reopenedRootBackend.text(id)) == "stored in local git"
            })
            XCTAssertTrue(reopenedMountedChildren.contains { id in
                (try? reopenedRootBackend.text(id)) == "edited through mount"
            })
            reopenedRootBackend.close()
        }
    }
}

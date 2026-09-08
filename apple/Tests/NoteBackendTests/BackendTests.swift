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
}

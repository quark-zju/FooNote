import Foundation
import XCTest
@testable import FooNoteApple

@MainActor
final class SavingEncryptionTests: XCTestCase {
    func testDelayedSaveFailureRecoveryAndRetry() async throws {
        let base = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        let parent = base.appendingPathComponent("missing")
        let path = parent.appendingPathComponent("notes.foonote")
        let model = Notebook(url: path.path, autosaveDelay: 50_000_000)
        defer { model.backend.close(); try? FileManager.default.removeItem(at: base) }
        model.add()
        model.draft = "Recover this unsaved text"
        try await Task.sleep(nanoseconds: 200_000_000)
        XCTAssertNotNil(model.saveError)
        XCTAssertTrue(model.dirty)
        model.exportRecovery()
        let recovery = try XCTUnwrap(model.recoveryURL)
        defer { try? FileManager.default.removeItem(at: recovery) }
        XCTAssertTrue(FileManager.default.fileExists(atPath: recovery.path))
        XCTAssertNotNil(model.saveError) // export is not a successful original save
        try FileManager.default.createDirectory(at: parent, withIntermediateDirectories: true)
        XCTAssertTrue(model.save())
        XCTAssertNil(model.saveError)
        XCTAssertFalse(model.dirty)
        XCTAssertTrue(FileManager.default.fileExists(atPath: path.path))
    }

    func testDelayResetsAfterAnotherEdit() async throws {
        let base = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: base, withIntermediateDirectories: true)
        let path = base.appendingPathComponent("notes.foonote")
        let model = Notebook(url: path.path, autosaveDelay: 200_000_000)
        defer { model.backend.close(); try? FileManager.default.removeItem(at: base) }
        model.add(); model.draft = "first edit"
        try await Task.sleep(nanoseconds: 120_000_000)
        model.draft = "second edit"
        try await Task.sleep(nanoseconds: 120_000_000)
        XCTAssertTrue(model.dirty)
        XCTAssertFalse(FileManager.default.fileExists(atPath: path.path))
        try await Task.sleep(nanoseconds: 200_000_000)
        XCTAssertFalse(model.dirty)
        XCTAssertTrue(FileManager.default.fileExists(atPath: path.path))
    }

    func testSnapshotFlattensExternalMount() async throws {
        let model = Notebook(url: "memory:")
        defer { model.backend.close() }
        XCTAssertTrue(model.connect(url: "memory:\(UUID().uuidString)", title: "Mounted"))
        model.add(child: true); model.draft = "external unsaved content"
        model.exportRecovery()
        let url = try XCTUnwrap(model.recoveryURL)
        defer { try? FileManager.default.removeItem(at: url) }
        model.backend.close()
        let root = try model.backend.open(url.path)
        let folder = try XCTUnwrap(model.backend.children(root).first)
        XCTAssertEqual(try model.backend.extractMeta(folder, prefix: "mount="), "")
        XCTAssertEqual(try model.backend.extractMeta(folder, prefix: "type="), "folder")
        let child = try XCTUnwrap(model.backend.children(folder).first)
        XCTAssertEqual(try model.backend.text(child), "external unsaved content")
    }

    func testEncryptedAreaLockAndRecovery() async throws {
        let model = Notebook(url: "memory:")
        defer { model.backend.close() }
        model.creatingEncryption = true
        XCTAssertTrue(model.unlockOrCreate(password: "correct", name: "Private"))
        let area = try XCTUnwrap(model.selected)
        XCTAssertTrue(try XCTUnwrap(model.find(area)).unlocked)
        model.add(child: true)
        model.draft = "SECRET that must stay encrypted"
        XCTAssertTrue(model.select([area]))
        model.exportRecovery() // ciphertext refreshed even without saving original
        let recovery = try XCTUnwrap(model.recoveryURL)
        defer { try? FileManager.default.removeItem(at: recovery) }
        let bytes = try Data(contentsOf: recovery)
        XCTAssertNil(bytes.range(of: Data("SECRET that must stay encrypted".utf8)))
        model.lockEncryption()
        XCTAssertFalse(try XCTUnwrap(model.find(area)).unlocked)
        XCTAssertTrue(try XCTUnwrap(model.find(area)).children.isEmpty)
        XCTAssertEqual(model.draft, "")
        model.creatingEncryption = false
        XCTAssertFalse(model.unlockOrCreate(password: "wrong"))
        XCTAssertTrue(model.unlockOrCreate(password: "correct"))
        XCTAssertEqual(try XCTUnwrap(model.find(area)).children.first?.title, "SECRET that must stay encrypted")
        model.backend.close()
        let restored = try model.backend.open(recovery.path)
        let restoredArea = try XCTUnwrap(model.backend.children(restored).first)
        try model.backend.updateMeta(restoredArea, prefix: "password=", value: "correct")
        XCTAssertEqual(try model.backend.extractMeta(restoredArea, prefix: "mounted="), "true")
        let restoredChild = try XCTUnwrap(model.backend.children(restoredArea).first)
        XCTAssertEqual(try model.backend.text(restoredChild), "SECRET that must stay encrypted")
    }
}

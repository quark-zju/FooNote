import XCTest
import NoteBackend
@testable import FooNoteApple

@MainActor
final class NotebookTests: XCTestCase {
    func testCreateKindsAndDraftFlush() async throws {
        let model = Notebook(url: "memory:")
        XCTAssertTrue(model.notes.isEmpty)

        model.add(kind: "folder")
        let folder = try XCTUnwrap(model.selected)
        XCTAssertEqual(model.find(folder)?.kind, "folder")
        model.add(kind: "note", child: true)
        let note = try XCTUnwrap(model.selected)
        XCTAssertEqual(model.find(note)?.kind, "note")
        model.draft = "edited title"
        model.add(kind: "separator")
        let separator = try XCTUnwrap(model.selected)
        XCTAssertEqual(model.find(separator)?.kind, "separator")
        XCTAssertFalse(model.canEdit)
        XCTAssertEqual(try model.backend.text(note), "edited title")
        XCTAssertEqual(model.find(folder)?.children.map(\.id), [note, separator])
    }

    func testMultiMoveCycleIsRejectedWithoutMutation() async throws {
        let model = Notebook(url: "memory:")
        model.add(kind: "folder")
        let folder = try XCTUnwrap(model.selected)
        model.add(kind: "note", child: true)
        let child = try XCTUnwrap(model.selected)
        model.selection = [folder, child]
        XCTAssertFalse(model.move([folder, child], destination: child, position: 0))
        XCTAssertEqual(try model.backend.parent(folder), try XCTUnwrap(model.root))
        XCTAssertEqual(try model.backend.parent(child), folder)
    }

    func testLiveTitleAutofillAndSearchSnapshot() async throws {
        let model = Notebook(url: "memory:")
        defer { model.backend.close() }
        model.add()
        model.draft = "1"
        let first = try XCTUnwrap(model.selected)
        XCTAssertEqual(model.find(first)?.title, "1")
        model.add()
        model.draft = "2"
        model.add()
        XCTAssertEqual(model.draft, "3")
        model.query = "1"
        model.hits = [SearchHit(id: first, line: "1")]
        model.selectSearchHit(first)
        model.draft = "Changed\nbody"
        XCTAssertEqual(model.find(first)?.title, "Changed")
        let revision = model.titleRevision
        model.draft = "Changed\nanother body"
        XCTAssertEqual(model.titleRevision, revision)
        XCTAssertEqual(model.query, "1")
        XCTAssertEqual(model.hits.map(\.id), [first])
        XCTAssertEqual(model.focusTarget, .editor)
    }

    func testDropPositionMapping() async throws {
        let a = NodeID(backend: 1, id: 1)
        let b = NodeID(backend: 1, id: 2)
        let c = NodeID(backend: 1, id: 3)
        let nested = NodeID(backend: 1, id: 4)
        let separator = Note(id: NodeID(backend: 1, id: 5), title: "", kind: "separator", children: [])
        let child = Note(id: nested, title: "nested", kind: "note", children: [])
        let first = Note(id: a, title: "a", kind: "folder", children: [child])
        let second = Note(id: b, title: "b", kind: "note", children: [])
        let third = Note(id: c, title: "c", kind: "note", children: [])
        let roots = [first, second, third, separator]

        XCTAssertEqual(NoteOutline.Coordinator.resolveDrop(ids: [], item: nil, index: 1, roots: roots)?.0, b)
        XCTAssertEqual(NoteOutline.Coordinator.resolveDrop(ids: [], item: nil, index: 4, roots: roots)?.1, 0)
        XCTAssertEqual(NoteOutline.Coordinator.resolveDrop(ids: [], item: first, index: 0)?.0, nested)
        XCTAssertEqual(NoteOutline.Coordinator.resolveDrop(ids: [], item: first, index: -1)?.0, a)
        XCTAssertNil(NoteOutline.Coordinator.resolveDrop(ids: [], item: separator, index: 0, roots: roots))

        // A dragged row is skipped when finding the next anchor.
        XCTAssertEqual(NoteOutline.Coordinator.resolveDrop(ids: [b], item: nil, index: 1, roots: roots)?.0, c)
        XCTAssertEqual(NoteOutline.Coordinator.resolveDrop(ids: [c, separator.id], item: nil, index: 2, roots: roots)?.0, nil)
    }
}

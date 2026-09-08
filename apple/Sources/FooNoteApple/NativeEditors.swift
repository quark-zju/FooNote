import AppKit
import SwiftUI
import NoteBackend

final class EditorTextView: NSTextView {
    var escape: (() -> Void)?
    override func cancelOperation(_ sender: Any?) { escape?() }
}

@MainActor
struct NoteEditor: NSViewRepresentable {
    @ObservedObject var model: Notebook
    func makeCoordinator() -> Coordinator { Coordinator(model) }
    func makeNSView(context: Context) -> NSScrollView {
        let scroll = NSScrollView()
        scroll.hasVerticalScroller = true; scroll.autohidesScrollers = true
        let text = EditorTextView()
        text.isRichText = false; text.allowsUndo = true
        text.font = .systemFont(ofSize: 14)
        text.textContainerInset = NSSize(width: 8, height: 10)
        text.isVerticallyResizable = true; text.isHorizontallyResizable = false
        text.autoresizingMask = [.width]
        text.textContainer?.widthTracksTextView = true
        text.textContainer?.containerSize = NSSize(width: 0, height: CGFloat.greatestFiniteMagnitude)
        text.delegate = context.coordinator
        text.escape = { model.editorEscape() }
        text.setAccessibilityLabel("Note text")
        scroll.documentView = text
        return scroll
    }
    func updateNSView(_ scroll: NSScrollView, context: Context) {
        guard let text = scroll.documentView as? EditorTextView else { return }
        context.coordinator.model = model
        if context.coordinator.node != model.selected {
            text.undoManager?.removeAllActions()
            context.coordinator.node = model.selected
        }
        if text.string != model.draft { text.string = model.draft }
        text.isEditable = model.canEdit
        if context.coordinator.focusRevision != model.focusRevision {
            context.coordinator.focusRevision = model.focusRevision
            if model.focusTarget == .editor && model.canEdit {
                DispatchQueue.main.async { text.window?.makeFirstResponder(text) }
            }
        }
    }
    @MainActor
    final class Coordinator: NSObject, NSTextViewDelegate {
        var model: Notebook
        var node: NoteBackend.NodeID?
        var focusRevision = -1
        init(_ model: Notebook) { self.model = model }
        func textDidChange(_ notification: Notification) {
            if let text = notification.object as? NSTextView { model.draft = text.string }
        }
    }
}

@MainActor
struct SearchField: NSViewRepresentable {
    @ObservedObject var model: Notebook
    func makeCoordinator() -> Coordinator { Coordinator(model) }
    func makeNSView(context: Context) -> NSSearchField {
        let field = NSSearchField()
        field.placeholderString = "Search notes  /"
        field.delegate = context.coordinator
        field.sendsSearchStringImmediately = true
        field.setAccessibilityLabel("Search notes")
        return field
    }
    func updateNSView(_ field: NSSearchField, context: Context) {
        context.coordinator.model = model
        if field.stringValue != model.query { field.stringValue = model.query }
        if context.coordinator.focusRevision != model.focusRevision {
            context.coordinator.focusRevision = model.focusRevision
            if model.focusTarget == .search {
                DispatchQueue.main.async { field.window?.makeFirstResponder(field) }
            }
        }
    }
    @MainActor
    final class Coordinator: NSObject, NSSearchFieldDelegate {
        var model: Notebook
        var focusRevision = -1
        init(_ model: Notebook) { self.model = model }
        func controlTextDidChange(_ notification: Notification) {
            if let field = notification.object as? NSSearchField { model.query = field.stringValue }
        }
        func control(_ control: NSControl, textView: NSTextView, doCommandBy selector: Selector) -> Bool {
            if selector == #selector(NSResponder.cancelOperation(_:)) {
                model.searchEscape(); return true
            }
            if selector == #selector(NSResponder.insertNewline(_:)) || selector == #selector(NSResponder.moveDown(_:)) {
                if let hit = model.hits.first { model.reveal(hit.id) }
                else if model.query.isEmpty { model.focus(.tree) }
                return true
            }
            return false
        }
    }
}

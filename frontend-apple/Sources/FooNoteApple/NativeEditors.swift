import AppKit
import SwiftUI
import NoteBackend

final class EditorTextView: NSTextView {
    var escape: (() -> Void)?
    override func cancelOperation(_ sender: Any?) { escape?() }

    override func doCommand(by selector: Selector) {
        if hasMarkedText() { super.doCommand(by: selector); return }
        if selector == #selector(NSResponder.insertTab(_:)) {
            guard isEditable else { return }
            if selectedRange().length > 0 { changeSelectedLines(indenting: true) }
            else { insertText("  ", replacementRange: selectedRange()) }
            return
        }
        if selector == #selector(NSResponder.insertBacktab(_:)), selectedRange().length > 0 {
            changeSelectedLines(indenting: false)
            return
        }
        if selector == #selector(NSResponder.insertNewline(_:)) {
            insertNewlineKeepingIndent()
            return
        }
        super.doCommand(by: selector)
    }

    private func insertNewlineKeepingIndent() {
        guard isEditable else { return }
        let ns = string as NSString
        let range = selectedRange()
        let point = min(range.location, ns.length)
        let lineStart = ns.lineRange(for: NSRange(location: point, length: 0)).location
        var cursor = lineStart
        while cursor < point, ns.character(at: cursor) == 32 { cursor += 1 }
        insertText("\n" + String(repeating: " ", count: cursor - lineStart), replacementRange: range)
    }

    private func changeSelectedLines(indenting: Bool) {
        guard isEditable else { return }
        let ns = string as NSString
        let selection = selectedRange()
        guard selection.length > 0, ns.length > 0 else { return }
        let start = min(selection.location, ns.length)
        let end = min(NSMaxRange(selection), ns.length)
        let firstLineStart = ns.lineRange(for: NSRange(location: start, length: 0)).location
        let probe = max(firstLineStart, min(end - 1, ns.length - 1))
        let lastLineEnd = NSMaxRange(ns.lineRange(for: NSRange(location: probe, length: 0)))
        let block = NSRange(location: firstLineStart, length: lastLineEnd - firstLineStart)

        var replacement = ""
        var editPositions: [(location: Int, delta: Int)] = []
        var cursor = block.location
        while cursor < NSMaxRange(block) {
            let lineRange = ns.lineRange(for: NSRange(location: cursor, length: 0))
            let lineStart = lineRange.location
            let lineEnd = min(NSMaxRange(lineRange), NSMaxRange(block))
            let spaces = spacesForOutdent(ns, at: lineStart)
            if indenting {
                replacement += "  "
                editPositions.append((lineStart, 2))
            } else {
                if spaces > 0 { editPositions.append((lineStart, -spaces)) }
            }
            let contentStart = lineStart + (indenting ? 0 : spaces)
            replacement += ns.substring(with: NSRange(location: contentStart, length: max(0, lineEnd - contentStart)))
            cursor = NSMaxRange(lineRange)
            if cursor >= NSMaxRange(block) { break }
        }

        guard shouldChangeText(in: block, replacementString: replacement) else { return }
        textStorage?.replaceCharacters(in: block, with: replacement)
        didChangeText()

        func adjusted(_ position: Int, inclusive: Bool) -> Int {
            position + editPositions.reduce(into: 0) { result, edit in
                if edit.delta < 0 {
                    result -= min(-edit.delta, max(0, position - edit.location))
                } else if edit.location < position || (inclusive && edit.location == position) {
                    result += edit.delta
                }
            }
        }
        let newStart = adjusted(start, inclusive: false)
        let newEnd = adjusted(end, inclusive: true)
        setSelectedRange(NSRange(location: newStart, length: max(0, newEnd - newStart)))
    }

    private func spacesForOutdent(_ ns: NSString, at location: Int) -> Int {
        var count = 0
        while count < 2, location + count < ns.length, ns.character(at: location + count) == 32 { count += 1 }
        return count
    }
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
                if let hit = model.hits.first { model.selectSearchHit(hit.id) }
                else if model.query.isEmpty { model.focus(.tree) }
                return true
            }
            return false
        }
    }
}

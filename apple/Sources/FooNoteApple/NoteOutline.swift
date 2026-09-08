import AppKit
import SwiftUI
import NoteBackend

/// The outline is deliberately kept as an AppKit view.  NSOutlineView still
/// provides the most useful combination of keyboard navigation, multiple
/// selection and row-oriented drag and drop on macOS.
@MainActor
struct NoteOutline: NSViewRepresentable {
    @ObservedObject var model: Notebook

    func makeCoordinator() -> Coordinator { Coordinator(model: model) }

    func makeNSView(context: Context) -> NSScrollView {
        let view = NoteOutlineView()
        context.coordinator.install(on: view)
        let scroll = NSScrollView()
        scroll.documentView = view
        scroll.hasVerticalScroller = true
        scroll.hasHorizontalScroller = false
        scroll.autohidesScrollers = true
        return scroll
    }

    func updateNSView(_ scroll: NSScrollView, context: Context) {
        context.coordinator.model = model
        guard let view = scroll.documentView as? NoteOutlineView else { return }
        context.coordinator.refresh(view)
    }

    @MainActor
    final class Coordinator: NSObject, NSOutlineViewDataSource, NSOutlineViewDelegate {
        var model: Notebook
        weak var outline: NoteOutlineView?
        private var refreshInProgress = false
        private var lastTreeRevision = -1
        private var lastTitleRevision = -1
        private var lastSelection: Set<NodeID> = []
        private var lastFocusRevision = -1
        private static let pasteboardType = NSPasteboard.PasteboardType("com.foonote.node-ids")

        init(model: Notebook) { self.model = model }

        func install(on view: NoteOutlineView) {
            outline = view
            view.delegate = self
            view.dataSource = self
            view.headerView = nil
            let column = NSTableColumn(identifier: NSUserInterfaceItemIdentifier("NoteColumn"))
            column.resizingMask = .autoresizingMask
            view.addTableColumn(column)
            view.outlineTableColumn = column
            view.autosaveTableColumns = false
            view.columnAutoresizingStyle = .uniformColumnAutoresizingStyle
            view.autoresizingMask = [.width]
            view.setDraggingSourceOperationMask(.move, forLocal: true)
            view.indentationPerLevel = 14
            view.frame = NSRect(x: 0, y: 0, width: 280, height: 400)
            view.allowsMultipleSelection = true
            view.allowsEmptySelection = true
            view.allowsColumnReordering = false
            view.rowSizeStyle = .small
            view.registerForDraggedTypes([Self.pasteboardType])
            view.keyHandler = { [weak self] key in self?.handle(key) ?? false }
            view.contextMenuHandler = { [weak self] row in self?.contextMenu(row: row) }
            view.doubleClickHandler = { [weak self] in self?.model.editSelected() }
            view.reloadData()
            lastTreeRevision = model.treeRevision
            lastSelection = model.selection
            applyFocus(view)
        }

        func refresh(_ view: NoteOutlineView) {
            guard !refreshInProgress else { return }
            refreshInProgress = true
            defer { refreshInProgress = false }
            let treeChanged = model.treeRevision != lastTreeRevision
            let selectionChanged = model.selection != lastSelection
            if treeChanged {
                let expanded = expandedIDs(in: view)
                view.reloadData()
                restore(expanded, in: view)
                lastTreeRevision = model.treeRevision
            }
            if selectionChanged || treeChanged {
                if selectionChanged { expandAncestors(of: model.selection, in: view) }
                syncSelection(in: view)
                revealSelection(in: view)
                lastSelection = model.selection
            }
            if model.titleRevision != lastTitleRevision {
                lastTitleRevision = model.titleRevision
                view.reloadData(forRowIndexes: IndexSet(integersIn: 0..<view.numberOfRows),
                    columnIndexes: IndexSet(integer: 0))
            }
            if model.focusRevision != lastFocusRevision {
                lastFocusRevision = model.focusRevision
                applyFocus(view)
            }
        }

        private func expandedIDs(in view: NSOutlineView) -> Set<NodeID> {
            var result = Set<NodeID>()
            for row in 0..<view.numberOfRows {
                guard view.isItemExpanded(view.item(atRow: row)),
                      let note = view.item(atRow: row) as? Note else { continue }
                result.insert(note.id)
            }
            return result
        }

        private func restore(_ expanded: Set<NodeID>, in view: NSOutlineView) {
            func walk(_ notes: [Note]) {
                for note in notes {
                    if expanded.contains(note.id) {
                        view.expandItem(note)
                    }
                    walk(note.children)
                }
            }
            walk(model.notes)
        }

        private func expandAncestors(of ids: Set<NodeID>, in view: NSOutlineView) {
            func walk(_ notes: [Note], ancestors: [Note]) {
                for note in notes {
                    if ids.contains(note.id) { ancestors.forEach { view.expandItem($0) } }
                    walk(note.children, ancestors: ancestors + [note])
                }
            }
            walk(model.notes, ancestors: [])
        }

        private func applyFocus(_ view: NoteOutlineView) {
            DispatchQueue.main.async { [weak view, weak self] in
                guard let view, let self, let window = view.window else { return }
                if self.model.focusTarget == .tree { window.makeFirstResponder(view) }
            }
        }

        private func find(_ id: NodeID, in notes: [Note]) -> Note? {
            for note in notes {
                if note.id == id { return note }
                if let found = find(id, in: note.children) { return found }
            }
            return nil
        }

        private func syncSelection(in view: NSOutlineView) {
            let rows = model.selection.compactMap { id in
                (0..<view.numberOfRows).first { (view.item(atRow: $0) as? Note)?.id == id }
            }
            view.selectRowIndexes(IndexSet(rows), byExtendingSelection: false)
        }

        private func revealSelection(in view: NSOutlineView) {
            for id in model.selection {
                if let note = find(id, in: model.notes) {
                    let row = view.row(forItem: note)
                    guard row >= 0 else { continue }
                    view.scrollRowToVisible(row)
                }
            }
        }

        // MARK: Data source
        func outlineView(_ outlineView: NSOutlineView, numberOfChildrenOfItem item: Any?) -> Int {
            (item as? Note)?.children.count ?? model.notes.count
        }

        func outlineView(_ outlineView: NSOutlineView, child index: Int, ofItem item: Any?) -> Any {
            (item as? Note)?.children[index] ?? model.notes[index]
        }

        func outlineView(_ outlineView: NSOutlineView, isItemExpandable item: Any) -> Bool {
            (item as? Note)?.children.isEmpty == false
        }

        func outlineView(_ outlineView: NSOutlineView, viewFor tableColumn: NSTableColumn?, item: Any) -> NSView? {
            let note = item as! Note
            let cell = (outlineView.makeView(withIdentifier: NSUserInterfaceItemIdentifier("NoteCell"), owner: self) as? NSTableCellView)
                ?? NSTableCellView()
            cell.identifier = NSUserInterfaceItemIdentifier("NoteCell")
            if cell.textField == nil {
                let text = NSTextField(labelWithString: "")
                let icon = NSImageView()
                text.lineBreakMode = .byTruncatingTail
                text.translatesAutoresizingMaskIntoConstraints = false
                icon.translatesAutoresizingMaskIntoConstraints = false
                cell.addSubview(text); cell.addSubview(icon)
                cell.textField = text; cell.imageView = icon
                NSLayoutConstraint.activate([
                    icon.leadingAnchor.constraint(equalTo: cell.leadingAnchor),
                    icon.centerYAnchor.constraint(equalTo: cell.centerYAnchor),
                    icon.widthAnchor.constraint(equalToConstant: 16),
                    icon.heightAnchor.constraint(equalToConstant: 16),
                    text.leadingAnchor.constraint(equalTo: icon.trailingAnchor, constant: 5),
                    text.trailingAnchor.constraint(equalTo: cell.trailingAnchor),
                    text.centerYAnchor.constraint(equalTo: cell.centerYAnchor)
                ])
            }
            let separator = note.kind == "separator"
            cell.textField?.stringValue = note.title
            cell.textField?.isHidden = separator
            cell.imageView?.isHidden = separator
            let symbol: String
            switch note.kind {
            case "mount": symbol = "externaldrive"
            case "folder": symbol = "folder"
            case "trash": symbol = "trash"
            default: symbol = note.children.isEmpty ? "note.text" : "folder"
            }
            cell.imageView?.image = NSImage(systemSymbolName: symbol, accessibilityDescription: note.kind)
            cell.imageView?.contentTintColor = .secondaryLabelColor
            cell.toolTip = nil
            cell.setAccessibilityLabel(separator ? "Separator" : note.title)
            let lineID = NSUserInterfaceItemIdentifier("Separator")
            if separator && !cell.subviews.contains(where: { $0.identifier == lineID }) {
                let line = NSBox()
                line.identifier = lineID; line.boxType = .separator
                line.translatesAutoresizingMaskIntoConstraints = false
                cell.addSubview(line)
                NSLayoutConstraint.activate([
                    line.leadingAnchor.constraint(equalTo: cell.leadingAnchor),
                    line.trailingAnchor.constraint(equalTo: cell.trailingAnchor),
                    line.centerYAnchor.constraint(equalTo: cell.centerYAnchor)
                ])
            }
            cell.subviews.filter { $0.identifier == lineID }.forEach { $0.isHidden = !separator }
            return cell
        }

        func outlineViewSelectionDidChange(_ notification: Notification) {
            guard let view = outline, !refreshInProgress else { return }
            let ids = Set(view.selectedRowIndexes.compactMap { (view.item(atRow: $0) as? Note)?.id })
            let old = model.selection
            if !model.select(ids) {
                refreshInProgress = true
                syncSelection(in: view)
                refreshInProgress = false
                lastSelection = old
            }
        }

        func outlineView(_ outlineView: NSOutlineView, pasteboardWriterForItem item: Any) -> NSPasteboardWriting? {
            guard let note = item as? Note else { return nil }
            let payload = "\(note.id.backend):\(note.id.id)"
            let p = NSPasteboardItem(); p.setString(payload, forType: Self.pasteboardType)
            return p
        }

        func outlineView(_ outlineView: NSOutlineView, validateDrop info: NSDraggingInfo, proposedItem item: Any?, proposedChildIndex index: Int) -> NSDragOperation {
            guard let source = info.draggingSource as? NSOutlineView, source === outlineView,
                  let ids = draggedIDs(info), !ids.isEmpty else { return [] }
            guard let target = Self.resolveDrop(ids: ids, item: item as? Note, index: index, roots: model.notes) else { return [] }
            if let destination = target.0,
               ids.contains(destination) || ids.contains(where: { isDescendant(destination, of: $0, in: model.notes) }) {
                return []
            }
            return .move
        }

        func outlineView(_ outlineView: NSOutlineView, acceptDrop info: NSDraggingInfo, item: Any?, childIndex index: Int) -> Bool {
            guard let source = info.draggingSource as? NSOutlineView, source === outlineView,
                  let ids = draggedIDs(info), !ids.isEmpty else { return false }
            guard let target = Self.resolveDrop(ids: ids, item: item as? Note, index: index, roots: model.notes) else { return false }
            return model.move(ids, destination: target.0, position: target.1)
        }

        private func draggedIDs(_ info: NSDraggingInfo) -> [NodeID]? {
            let items = info.draggingPasteboard.pasteboardItems ?? []
            let values = items.flatMap { ($0.string(forType: Self.pasteboardType) ?? "").split(separator: "\n") }
            let ids = values.compactMap { part -> NodeID? in
                let values = part.split(separator: ":")
                guard values.count == 2, let backend = Int32(values[0]), let id = Int32(values[1]) else { return nil }
                return NodeID(backend: backend, id: id)
            }
            guard !ids.isEmpty else { return nil }
            var unique: [NodeID] = []
            var seen = Set<NodeID>()
            for id in ids where seen.insert(id).inserted { unique.append(id) }
            return unique
        }

        /// AppKit gives a parent plus insertion index; Rust expects a node anchor
        /// and Before (-1), After (1), or Append (0). Never pass an index as a position.
        static func resolveDrop(ids: [NodeID], item: Note?, index: Int, roots: [Note] = []) -> (NodeID?, Int32)? {
            if item?.kind == "separator" { return nil }
            if index < 0 { return (item?.id, 0) }
            let children = item?.children ?? roots
            guard index <= children.count else { return nil }
            var anchor = index
            let moving = Set(ids)
            while anchor < children.count && moving.contains(children[anchor].id) { anchor += 1 }
            if anchor < children.count { return (children[anchor].id, -1) }
            return (item?.id, 0)
        }

        private func isDescendant(_ candidate: NodeID, of ancestor: NodeID, in notes: [Note]) -> Bool {
            guard let parent = find(ancestor, in: notes) else { return false }
            func contains(_ id: NodeID, in children: [Note]) -> Bool {
                children.contains { $0.id == id || contains(id, in: $0.children) }
            }
            return contains(candidate, in: parent.children)
        }

        func contextMenu(row: Int) -> NSMenu? {
            guard let view = outline else { return nil }
            if let note = view.item(atRow: row) as? Note {
                if !model.selection.contains(note.id), !model.select([note.id]) { return nil }
            } else if !model.select([]) { return nil }
            let menu = NSMenu()
            menu.autoenablesItems = false
            func add(_ title: String, _ action: Selector, enabled: Bool = true) {
                let item = NSMenuItem(title: title, action: action, keyEquivalent: "")
                item.target = self; item.isEnabled = enabled; menu.addItem(item)
            }
            add("Edit Note", #selector(editNote), enabled: model.canEdit)
            menu.addItem(.separator())
            add("New Note", #selector(newNote))
            add("New Folder", #selector(newFolder))
            add("New Child Note", #selector(newChild), enabled: model.selected != nil && model.selectedNote?.kind != "separator")
            add("New Separator", #selector(newSeparator))
            add("Mount Notebook / Git…", #selector(mountNotebook))
            menu.addItem(.separator())
            add("Expand All", #selector(expandAll))
            add("Collapse All", #selector(collapseAll))
            menu.addItem(.separator())
            add("Delete Selected", #selector(deleteSelected), enabled: !model.selection.isEmpty)
            return menu
        }
        @objc private func editNote() { model.editSelected() }
        @objc private func newNote() { model.add() }
        @objc private func newFolder() { model.add(kind: "folder") }
        @objc private func newChild() { model.add(child: true) }
        @objc private func newSeparator() { model.add(kind: "separator") }
        @objc private func mountNotebook() { model.connectionIsRoot = false; model.showConnection = true }
        @objc private func expandAll() { outline?.expandItem(nil, expandChildren: true) }
        @objc private func collapseAll() { outline?.collapseItem(nil, collapseChildren: true) }
        @objc private func deleteSelected() { model.requestDelete() }

        private func handle(_ key: NoteOutlineView.Key) -> Bool {
            switch key {
            case .return: model.editSelected(); return true
            case .slash: model.focusSearch(); return true
            case .escape: model.treeEscape(); return true
            case .delete: model.requestDelete(); return true
            }
        }
    }
}

@MainActor
final class NoteOutlineView: NSOutlineView {
    enum Key { case `return`, slash, escape, delete }
    var keyHandler: ((Key) -> Bool)?
    var contextMenuHandler: ((Int) -> NSMenu?)?
    override func menu(for event: NSEvent) -> NSMenu? {
        contextMenuHandler?(row(at: convert(event.locationInWindow, from: nil)))
    }

    override func keyDown(with event: NSEvent) {
        if event.modifierFlags.intersection([.command, .control, .option]).isEmpty == false {
            super.keyDown(with: event)
            return
        }
        switch event.keyCode {
        case 36, 76: if keyHandler?(.return) == true { return }
        case 53: if keyHandler?(.escape) == true { return }
        case 51, 117: if keyHandler?(.delete) == true { return }
        default: break
        }
        if event.charactersIgnoringModifiers == "/", keyHandler?(.slash) == true { return }
        super.keyDown(with: event)
    }

    var doubleClickHandler: (() -> Void)?

    override func mouseDown(with event: NSEvent) {
        super.mouseDown(with: event)
        if event.clickCount == 2 { doubleClickHandler?() }
    }
}

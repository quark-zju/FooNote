import AppKit
import SwiftUI
import NoteBackend

enum FocusTarget { case tree, editor, search }

final class Note: Identifiable {
    let id: NodeID
    var title: String
    var kind: String
    var children: [Note]
    let readOnly: Bool
    var encrypted = false
    var unlocked = false
    init(id: NodeID, title: String, kind: String, children: [Note], readOnly: Bool = false) {
        self.id = id; self.title = title; self.kind = kind
        self.children = children; self.readOnly = readOnly
    }
}

struct SearchHit: Identifiable {
    let id: NodeID
    let line: String
}

@MainActor
final class Notebook: ObservableObject {
    let backend = Backend()
    @Published var notes: [Note] = []
    @Published var selection: Set<NodeID> = []
    @Published var draft = "" { didSet {
        updateTitle()
        if !loadingDraft && draft != oldValue { changed() }
    } }
    @Published var titleRevision = 0
    @Published var error: String?
    @Published var saveError: String?
    @Published var recoveryURL: URL?
    @Published var dirty = false
    @Published var showEncryption = false
    @Published var creatingEncryption = false
    private var loadingDraft = false
    private var autosaveTask: Task<Void, Never>?
    private let autosaveDelay: UInt64
    @Published var location = ""
    @Published var status = "Ready"
    @Published var treeRevision = 0
    @Published var focusTarget = FocusTarget.tree
    @Published var focusRevision = 0
    @Published var query = ""
    @Published var hits: [SearchHit] = []
    @Published var searching = false
    @Published var confirmDelete = false
    @Published var showConnection = false
    @Published var connectionIsRoot = false
    private(set) var root: NodeID?
    private(set) var selected: NodeID?
    private var savedText = ""
    private let remembersLocation: Bool
    private var searchTask: Task<Void, Never>?
    var selectedNote: Note? { selected.flatMap { find($0) } }
    var canEdit: Bool { selected != nil && selectedNote?.readOnly != true }

    init(url: String? = nil, autosaveDelay: UInt64 = 30_000_000_000) {
        self.autosaveDelay = autosaveDelay
        remembersLocation = url == nil
        if let url { open(url); return }
        do {
            let directory = try FileManager.default.url(for: .applicationSupportDirectory,
                in: .userDomainMask, appropriateFor: nil, create: true)
                .appendingPathComponent("FooNoteApple", isDirectory: true)
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
            let defaultPath = directory.appendingPathComponent("Prototype.foonote").path
            open(UserDefaults.standard.string(forKey: "notebookURL") ?? defaultPath)
        } catch { self.error = error.localizedDescription }
    }

    @discardableResult func attempt(_ action: () throws -> Void) -> Bool {
        do { try action(); return true }
        catch { self.error = error.localizedDescription; status = "Operation failed"; return false }
    }

    func find(_ id: NodeID) -> Note? {
        func walk(_ nodes: [Note]) -> Note? {
            for node in nodes {
                if node.id == id { return node }
                if let found = walk(node.children) { return found }
            }
            return nil
        }
        return walk(notes)
    }

    func reload() throws {
        guard let root else { return }
        func load(_ id: NodeID, ancestors: Set<NodeID>) throws -> Note {
            let text = try backend.text(id)
            let meta = try backend.metadata(id).components(separatedBy: .newlines)
            let kind = meta.first(where: { $0.hasPrefix("type=") }).map { String($0.dropFirst(5)) } ?? "note"
            let title = text.components(separatedBy: .newlines).first.flatMap { $0.isEmpty ? nil : $0 }
                ?? (kind == "folder" ? "Untitled folder" : "Untitled")
            let mountURL = meta.first(where: { $0.hasPrefix("mount=") }).map { String($0.dropFirst(6)) } ?? ""
            let encrypted = mountURL == "aes256" || mountURL.hasPrefix("aes256:")
            let node = Note(id: id, title: title, kind: kind, children: [],
                readOnly: meta.contains("readonly=true") || kind == "separator" || encrypted)
            node.encrypted = encrypted
            node.unlocked = try encrypted && backend.extractMeta(id, prefix: "mounted=") == "true"
            // Repeated mount aliases can lead back to an ancestor; show the alias but do not recurse.
            if !ancestors.contains(id) && (!encrypted || node.unlocked) {
                node.children = try backend.children(id).map { try load($0, ancestors: ancestors.union([id])) }
            }
            return node
        }
        notes = try backend.children(root).map { try load($0, ancestors: [root]) }
        treeRevision += 1
    }

    private func updateTitle() {
        guard let note = selectedNote, !note.readOnly else { return }
        let first = draft.components(separatedBy: .newlines).first ?? ""
        let title = first.isEmpty ? (note.kind == "folder" ? "Untitled folder" : "Untitled") : first
        if note.title != title { note.title = title; titleRevision += 1 }
    }

    func selectSearchHit(_ id: NodeID) {
        // Keep the result list as a snapshot while editing its notes.
        stopSearch()
        if select([id]) { focus(.editor) }
    }

    private func flushDraft() throws {
        if let selected, draft != savedText, canEdit {
            try backend.setText(selected, text: draft)
            savedText = draft
        }
    }

    private func loadDraft(_ text: String) {
        loadingDraft = true
        draft = text; savedText = text
        loadingDraft = false
    }

    func changed() {
        guard root != nil else { return }
        dirty = true
        status = "Unsaved — saving in 30s"
        autosaveTask?.cancel()
        let delay = autosaveDelay
        autosaveTask = Task { [weak self] in
            do { try await Task.sleep(nanoseconds: delay) } catch { return }
            guard !Task.isCancelled else { return }
            self?.save()
        }
    }

    @discardableResult func save() -> Bool {
        guard root != nil else { return true }
        autosaveTask?.cancel(); autosaveTask = nil
        do {
            try flushDraft()
            try backend.persist()
            dirty = false; saveError = nil
            try reload()
            status = "Saved / synced"
            return true
        } catch {
            dirty = true; saveError = error.localizedDescription
            status = "Save failed"
            return false
        }
    }

    func exportRecovery() {
        attempt {
            try flushDraft()
            let data = try backend.exportSnapshot()
            let url = FileManager.default.temporaryDirectory
                .appendingPathComponent("FooNote-Recovery-\(UUID().uuidString).foonote")
            try data.write(to: url, options: [.atomic])
            recoveryURL = url
        }
    }

    func requestEncryption(create: Bool) {
        creatingEncryption = create
        showEncryption = true
    }

    @discardableResult func unlockOrCreate(password: String, name: String = "Encrypted") -> Bool {
        guard !password.isEmpty else { error = "Password cannot be empty."; return false }
        return attempt {
            try flushDraft()
            let id: NodeID
            if creatingEncryption {
                guard let (destination, position) = insertion(child: false) else { return }
                id = try backend.insert(parent: destination,
                    text: (name.isEmpty ? "Encrypted" : name.components(separatedBy: .newlines)[0]) + "\n\nDo not edit lines below:\n",
                    meta: "type=mount\nmount=aes256:\(UUID().uuidString)\ncopyable=false\n", position: position)
                changed()
            } else {
                guard let selected, selectedNote?.encrypted == true else { return }
                id = selected
            }
            try backend.updateMeta(id, prefix: "password=", value: password)
            guard try backend.extractMeta(id, prefix: "mounted=") == "true" else {
                throw NSError(domain: "FooNote", code: 1,
                    userInfo: [NSLocalizedDescriptionKey: "Cannot unlock encrypted area. Check the password."])
            }
            clearSearch()
            selected = id; selection = [id]; loadDraft("")
            try reload()
            focus(.tree)
            if creatingEncryption { changed() }
        }
    }

    func lockEncryption() {
        guard let id = selected, selectedNote?.encrypted == true else { return }
        attempt {
            try flushDraft()
            try backend.updateMeta(id, prefix: "password=", value: "")
            // Remove decrypted search results and editor buffers along with the mounted subtree.
            clearSearch(); loadDraft("")
            try reload(); changed(); focus(.tree)
        }
    }

    @discardableResult func select(_ ids: Set<NodeID>) -> Bool {
        if ids == selection { return true }
        return attempt {
            try flushDraft()
            try reload()
            // Persist explicitly with Cmd-S; navigation only updates backend memory (no Git push per click).
            let ordered = orderedIDs(ids)
            let next = ids.contains(selected ?? NodeID(backend: -1, id: -1)) ? selected : ordered.first
            let text = try next.map { find($0)?.encrypted == true ? "" : try backend.text($0) } ?? ""
            selected = next; selection = ids; loadDraft(text)
        }
    }

    func orderedIDs(_ ids: Set<NodeID>) -> [NodeID] {
        var result: [NodeID] = []
        func walk(_ nodes: [Note]) {
            for node in nodes {
                if ids.contains(node.id) { result.append(node.id) }
                walk(node.children)
            }
        }
        walk(notes)
        return result
    }

    @discardableResult func open(_ path: String) -> Bool {
        guard save() else { return false }
        if path == location { return true }
        stopSearch()
        return attempt {
            let newRoot = try backend.open(path)
            root = newRoot; selected = nil; selection = []; loadDraft("")
            location = path; query = ""; hits = []; notes = []
            try reload()
            if remembersLocation { UserDefaults.standard.set(path, forKey: "notebookURL") }
            status = "Opened"; focus(.tree)
        }
    }

    func chooseFile(create: Bool) {
        if create {
            let panel = NSSavePanel()
            panel.title = "New FooNote notebook"; panel.nameFieldStringValue = "Notes.foonote"
            if panel.runModal() == .OK, let url = panel.url {
                let path = url.pathExtension == "foonote" ? url.path : url.path + ".foonote"
                if open(path) { _ = save() }
            }
        } else {
            let panel = NSOpenPanel()
            panel.allowsMultipleSelection = false; panel.canChooseDirectories = false
            panel.message = "Choose a .foonote notebook. Use Open Root URL for Git."
            if panel.runModal() == .OK, let url = panel.url { open(url.path) }
        }
    }

    func insertion(child: Bool) -> (NodeID, Int32)? {
        guard let root else { return nil }
        guard let selected else { return (root, 0) }
        if child || ["folder", "mount", "trash"].contains(selectedNote?.kind ?? "") {
            return (selected, 0)
        }
        return (selected, 1)
    }

    func add(kind: String = "note", child: Bool = false) {
        guard let (destination, position) = insertion(child: child) else { return }
        attempt {
            try flushDraft()
            let meta = kind == "note" ? "" : "type=\(kind)\n" + (kind == "separator" ? "readonly=true\n" : "")
            let id = try backend.insert(parent: destination, text: "", meta: meta, position: position)
            if kind != "separator" { try backend.autofill(id) }
            let text = try backend.text(id)
            try reload()
            selected = id; selection = [id]; loadDraft(text)
            clearSearch(); focus(kind == "separator" ? .tree : .editor)
            changed()
        }
    }

    func connect(url: String, title: String) -> Bool {
        let value = url.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !value.isEmpty, !value.contains("\n"), !value.contains("\r") else {
            error = "Enter a notebook path or Git URL on one line."; return false
        }
        if connectionIsRoot { return open(value) }
        guard let (destination, position) = insertion(child: false) else { return false }
        return attempt {
            try flushDraft()
            let id = try backend.insert(parent: destination, text: title.isEmpty ? value : title,
                meta: "type=mount\nmount=\(value)\n", position: position)
            try reload()
            selected = id; selection = [id]; loadDraft(try backend.text(id))
            clearSearch(); focus(.tree); changed()
        }
    }

    @discardableResult func move(_ ids: [NodeID], destination: NodeID?, position: Int32) -> Bool {
        guard let target = destination ?? root, !ids.isEmpty else { return false }
        return attempt {
            try flushDraft()
            let moving = Set(ids)
            guard !moving.contains(target) else { throw NSError(domain: "FooNote", code: 1,
                userInfo: [NSLocalizedDescriptionKey: "Choose a destination outside the selection."]) }
            var ancestor = position == 0 ? target : try backend.parent(target)
            var seen: Set<NodeID> = []
            while seen.insert(ancestor).inserted {
                if moving.contains(ancestor) { throw NSError(domain: "FooNote", code: 1,
                    userInfo: [NSLocalizedDescriptionKey: "A node cannot be moved inside itself or its descendants."]) }
                let parent = try backend.parent(ancestor)
                if parent == ancestor { break }; ancestor = parent
            }
            let ordered = orderedIDs(moving)
            selected = nil; selection = []; loadDraft("")
            let moved: [NodeID]
            do { moved = try backend.move(ordered, destination: target, position: position) }
            catch { try? reload(); throw error }
            try reload()
            _ = select(Set(moved)); focus(.tree)
            changed()
        }
    }

    func requestDelete() { if !selection.isEmpty { confirmDelete = true } }
    func delete() {
        attempt {
            try flushDraft()
            let removing = orderedIDs(selection)
            selected = nil; selection = []; loadDraft("")
            do { try backend.remove(removing) }
            catch { try? reload(); throw error }
            try reload(); refreshSearch(); changed()
        }
    }

    func focus(_ target: FocusTarget) { focusTarget = target; focusRevision += 1 }
    func editSelected() {
        if selectedNote?.encrypted == true && selectedNote?.unlocked == false { requestEncryption(create: false) }
        else if canEdit { focus(.editor) }
    }
    func focusSearch() { focus(.search) }
    func treeEscape() { focusSearch() }
    func editorEscape() {
        if attempt({ try flushDraft(); try reload() }) { focus(.tree) }
    }
    func clearSearch() { query = ""; stopSearch(); hits = [] }
    func searchEscape() { clearSearch(); focus(.tree) }
    func stopSearch() {
        searchTask?.cancel(); searchTask = nil
        try? backend.searchStop(); searching = false
    }
    func refreshSearch() {
        stopSearch(); hits = []
        guard !query.isEmpty, let root else { return }
        searching = true
        let text = query
        searchTask = Task { [weak self] in
            do {
                try await Task.sleep(nanoseconds: 180_000_000)
                guard let self, !Task.isCancelled else { return }
                try self.flushDraft()
                try self.backend.searchStart(text, roots: [root])
                while !Task.isCancelled {
                    let complete = try self.backend.searchComplete()
                    var seen: Set<NodeID> = []
                    self.hits = try self.backend.searchResults().compactMap { id, line in
                        seen.insert(id).inserted ? SearchHit(id: id, line: line) : nil
                    }
                    if complete { self.searching = false; break }
                    try await Task.sleep(nanoseconds: 80_000_000)
                }
            } catch is CancellationError { }
            catch { self?.error = error.localizedDescription; self?.searching = false }
        }
    }
    func reveal(_ id: NodeID) {
        if select([id]) { clearSearch(); focus(.tree) }
    }
}

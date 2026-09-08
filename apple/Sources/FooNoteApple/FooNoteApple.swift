import AppKit
import SwiftUI
import NoteBackend

struct Note: Identifiable {
    let id: NodeID
    var title: String
    var children: [Note]?
}

@MainActor
final class Notebook: ObservableObject {
    let backend = Backend()
    @Published var notes: [Note] = []
    @Published var selected: NodeID?
    @Published var draft = ""
    @Published var error: String?
    @Published var location = ""
    @Published var status = "Ready"
    private var root: NodeID?
    private var savedText = ""

    init() {
        do {
            let directory = try FileManager.default.url(for: .applicationSupportDirectory,
                in: .userDomainMask, appropriateFor: nil, create: true)
                .appendingPathComponent("FooNoteApple", isDirectory: true)
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
            open(directory.appendingPathComponent("Prototype.foonote").path)
        } catch { self.error = error.localizedDescription }
    }

    @discardableResult func attempt(_ action: () throws -> Void) -> Bool {
        do { try action(); return true }
        catch { self.error = error.localizedDescription; status = "Operation failed"; return false }
    }

    func reload() throws {
        guard let root else { return }
        func load(_ id: NodeID) throws -> Note {
            let text = try backend.text(id)
            let children = try backend.children(id).map(load)
            return Note(id: id, title: text.components(separatedBy: .newlines).first.flatMap {
                $0.isEmpty ? nil : $0
            } ?? "Untitled", children: children.isEmpty ? nil : children)
        }
        notes = try backend.children(root).map(load)
    }

    @discardableResult func save() -> Bool {
        attempt {
            if let selected, draft != savedText { try backend.setText(selected, text: draft) }
            try backend.persist()
            savedText = draft
            try reload()
            status = "Saved"
        }
    }

    func select(_ id: NodeID?) {
        guard id != selected, save() else { return }
        attempt {
            let text = try id.map { try backend.text($0) } ?? ""
            selected = id; draft = text; savedText = text
        }
    }

    func open(_ path: String) {
        if root != nil && !save() { return }
        attempt {
            let newRoot = try backend.open(path)
            root = newRoot; selected = nil; draft = ""; savedText = ""
            location = path
            try reload()
            status = "Opened"
        }
    }

    func chooseFile(create: Bool) {
        if create {
            let panel = NSSavePanel()
            panel.title = "New FooNote notebook"
            panel.nameFieldStringValue = "Notes.foonote"
            if panel.runModal() == .OK, let url = panel.url {
                let path = url.pathExtension == "foonote" ? url.path : url.path + ".foonote"
                open(path)
                if location == path { _ = save() }
            }
        } else {
            let panel = NSOpenPanel()
            panel.allowsMultipleSelection = false
            panel.canChooseDirectories = false
            panel.message = "Choose a .foonote notebook"
            if panel.runModal() == .OK, let url = panel.url { open(url.path) }
        }
    }

    func add(child: Bool) {
        guard let parent = child ? selected : root, save() else { return }
        attempt {
            let id = try backend.insert(parent: parent, text: "New note")
            selected = id; draft = "New note"; savedText = draft
            try backend.persist(); try reload(); status = "Created"
        }
    }

    func delete() {
        guard let selected, save() else { return }
        attempt {
            try backend.remove(selected)
            self.selected = nil; draft = ""; savedText = ""
            try backend.persist(); try reload(); status = "Deleted"
        }
    }
}

@MainActor
final class AppDelegate: NSObject, NSApplicationDelegate {
    weak var notebook: Notebook?
    func applicationShouldTerminate(_ sender: NSApplication) -> NSApplication.TerminateReply {
        notebook?.save() == false ? .terminateCancel : .terminateNow
    }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

@main
struct FooNoteAppleApp: App {
    @NSApplicationDelegateAdaptor(AppDelegate.self) var delegate
    @StateObject private var notebook = Notebook()
    var body: some Scene {
        Window("FooNote", id: "notebook") {
            ContentView(model: notebook)
                .onAppear {
                    delegate.notebook = notebook
                    NSApplication.shared.setActivationPolicy(.regular)
                    NSApplication.shared.activate(ignoringOtherApps: true)
                }
        }
        .defaultSize(width: 960, height: 640)
        .commands {
            CommandGroup(replacing: .newItem) {
                Button("New Notebook…") { notebook.chooseFile(create: true) }.keyboardShortcut("n")
                Button("Open Notebook…") { notebook.chooseFile(create: false) }.keyboardShortcut("o")
            }
            CommandGroup(replacing: .saveItem) {
                Button("Save") { notebook.save() }.keyboardShortcut("s")
            }
        }
    }
}

struct ContentView: View {
    @ObservedObject var model: Notebook
    @State private var confirmDelete = false
    var body: some View {
        NavigationSplitView {
            List(selection: Binding(get: { model.selected }, set: { model.select($0) })) {
                OutlineGroup(model.notes, children: \.children) { note in
                    Label(note.title, systemImage: note.children == nil ? "note.text" : "folder")
                        .lineLimit(1).tag(note.id)
                }
            }
            .navigationTitle("Notes")
            .navigationSplitViewColumnWidth(min: 200, ideal: 260)
        } detail: {
            if model.selected != nil {
                VStack(alignment: .leading, spacing: 0) {
                    Text(model.draft.components(separatedBy: .newlines).first ?? "Untitled")
                        .font(.title2.bold()).lineLimit(1).padding()
                    Divider()
                    TextEditor(text: $model.draft)
                        .font(.system(size: 15)).padding(12)
                        .accessibilityLabel("Note text")
                    Divider()
                    HStack {
                        Text(model.status)
                        Spacer()
                        Text("\(model.draft.count) characters")
                    }.font(.caption).foregroundStyle(.secondary).padding(10)
                }
            } else {
                VStack(spacing: 14) {
                    Image(systemName: "square.and.pencil").font(.system(size: 44)).foregroundStyle(.secondary)
                    Text("A little space for your thoughts").font(.title2)
                    Text("Select a note, or create one to get started.").foregroundStyle(.secondary)
                    Button("New Note") { model.add(child: false) }
                }.frame(maxWidth: .infinity, maxHeight: .infinity)
            }
        }
        .frame(minWidth: 640, minHeight: 420)
        .navigationSubtitle(URL(fileURLWithPath: model.location).lastPathComponent)
        .toolbar {
            Button { model.chooseFile(create: false) } label: { Label("Open", systemImage: "folder") }
            Button { model.add(child: false) } label: { Label("New Note", systemImage: "square.and.pencil") }
            Button { model.add(child: true) } label: { Label("New Child", systemImage: "text.badge.plus") }
                .disabled(model.selected == nil)
            Button { model.save() } label: { Label("Save", systemImage: "square.and.arrow.down") }
            Button { confirmDelete = true } label: { Label("Delete", systemImage: "trash") }
                .disabled(model.selected == nil)
        }
        .confirmationDialog("Delete this note and its children?", isPresented: $confirmDelete) {
            Button("Delete", role: .destructive) { model.delete() }
        }
        .alert("FooNote", isPresented: Binding(get: { model.error != nil }, set: { if !$0 { model.error = nil } })) {
            Button("OK") { model.error = nil }
        } message: { Text(model.error ?? "") }
    }
}

import AppKit
import SwiftUI

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
                .background(WindowPlacement())
                .background(WindowAccessor(model: notebook))
                .onAppear {
                    delegate.notebook = notebook
                    NSApplication.shared.setActivationPolicy(.regular)
                    NSApplication.shared.activate(ignoringOtherApps: true)
                }
        }
        .defaultSize(width: 280, height: 760)
        .commands {
            CommandGroup(replacing: .newItem) {
                Button("New Note") { notebook.add() }.keyboardShortcut("n")
                Button("New Encrypted Area…") { notebook.requestEncryption(create: true) }
                Button("New Folder") { notebook.add(kind: "folder") }.keyboardShortcut("n", modifiers: [.command, .shift])
                Button("New Child Note") { notebook.add(child: true) }.keyboardShortcut("n", modifiers: [.command, .option])
                Button("New Separator") { notebook.add(kind: "separator") }.keyboardShortcut("=", modifiers: [.command, .shift])
                Divider()
                Button("New Notebook…") { notebook.chooseFile(create: true) }.keyboardShortcut("n", modifiers: [.command, .option, .shift])
                Button("Open Notebook…") { notebook.chooseFile(create: false) }.keyboardShortcut("o")
                Button("Open Root URL…") { notebook.connectionIsRoot = true; notebook.showConnection = true }
                Button("Mount Notebook / Git…") { notebook.connectionIsRoot = false; notebook.showConnection = true }
            }
            CommandGroup(replacing: .saveItem) {
                Button("Save / Sync") { notebook.save() }.keyboardShortcut("s")
            }
            CommandGroup(after: .textEditing) {
                Button("Find Notes") { notebook.focusSearch() }.keyboardShortcut("f")
            }
            CommandGroup(after: .windowSize) {
                Toggle("Always on Top", isOn: Binding(
                    get: { notebook.alwaysOnTop },
                    set: { notebook.setAlwaysOnTop($0) }
                ))
            }
        }
    }
}

/// Captures the hosting NSWindow so window-level settings (always on top) can be
/// applied at launch and re-applied whenever the setting changes.
private struct WindowAccessor: NSViewRepresentable {
    var model: Notebook
    final class Anchor: NSView {
        var model: Notebook?
        override func viewDidMoveToWindow() {
            super.viewDidMoveToWindow()
            guard let window else { return }
            model?.window = window
            model?.applyAlwaysOnTop()
        }
    }
    func makeNSView(context: Context) -> Anchor {
        let anchor = Anchor()
        anchor.model = model
        return anchor
    }
    func updateNSView(_ view: Anchor, context: Context) {
        if view.model !== model { view.model = model }
    }
}

struct ContentView: View {
    @ObservedObject var model: Notebook
    var body: some View {
        VStack(spacing: 0) {
            SearchField(model: model).frame(height: 26).padding(8)
            VSplitView {
                ZStack {
                    NoteOutline(model: model)
                        .opacity(model.query.isEmpty ? 1 : 0)
                        .allowsHitTesting(model.query.isEmpty)
                    if !model.query.isEmpty {
                        VStack(spacing: 0) {
                            HStack {
                                Text(model.searching ? "Searching…" : "\(model.hits.count) results")
                                Spacer()
                                Button("Clear") { model.searchEscape() }.buttonStyle(.borderless)
                            }.font(.caption).foregroundStyle(.secondary).padding(8)
                            List(model.hits) { hit in
                                Button { model.selectSearchHit(hit.id) } label: {
                                    VStack(alignment: .leading, spacing: 3) {
                                        Text(model.find(hit.id)?.title ?? "Note").fontWeight(.medium)
                                        Text(hit.line).font(.caption).foregroundStyle(.secondary).lineLimit(2)
                                    }.frame(maxWidth: .infinity, alignment: .leading)
                                }.buttonStyle(.plain)
                                .listRowBackground(model.selected == hit.id ? Color.accentColor.opacity(0.15) : Color.clear)
                            }
                            .overlay {
                                if model.hits.isEmpty && !model.searching { Text("No matching notes").foregroundStyle(.secondary) }
                            }
                        }
                    }
                }.frame(minHeight: 130, idealHeight: 360)
                VStack(spacing: 0) {
                    NoteEditor(model: model)
                        .overlay {
                            if model.selected == nil || model.selectedNote?.encrypted == true {
                                Text(model.selectedNote?.encrypted == true ? (model.selectedNote?.unlocked == true ? "Encrypted area unlocked — select a child note" : "Encrypted area locked — double-click to unlock") : "Select a note or press ⌘N").font(.callout).foregroundStyle(.secondary)
                                    .allowsHitTesting(false)
                            }
                        }
                }.frame(minHeight: 120, idealHeight: 300)
            }
            if let failure = model.saveError {
                VStack(alignment: .leading, spacing: 6) {
                    Text("Save failed").fontWeight(.semibold)
                    Text(failure).lineLimit(3).textSelection(.enabled)
                    HStack {
                        Button("Retry") { model.save() }
                        Button("Export Recovery…") { model.exportRecovery() }
                    }
                    if let url = model.recoveryURL {
                        Text(url.path).textSelection(.enabled).lineLimit(3)
                        Button("Show Recovery File") { NSWorkspace.shared.activateFileViewerSelecting([url]) }
                    }
                }.font(.caption).padding(12).frame(maxWidth: .infinity, alignment: .leading)
                    .background(Color.orange.opacity(0.12))
            }
            if model.selectedNote?.readOnly == true || model.saveError != nil {
                HStack {
                    if model.selectedNote?.readOnly == true {
                        Image(systemName: "lock").help("Read-only").accessibilityLabel("Read-only")
                    }
                    if model.saveError != nil {
                        Image(systemName: "exclamationmark.triangle")
                        Text("Save failed").lineLimit(1)
                    }
                    Spacer(minLength: 4)
                }.font(.caption2).foregroundStyle(.secondary)
                    .padding(.horizontal, 16).padding(.top, 8).padding(.bottom, 14)
            }
        }
        .frame(minWidth: 260, minHeight: 380)
        .navigationSubtitle(model.location.components(separatedBy: "/").last ?? "FooNote")
        .toolbar {
            ToolbarItemGroup {
                Menu {
                    Button("New Note  ⌘N") { model.add() }
                    Button("New Encrypted Area…") { model.requestEncryption(create: true) }
                    if model.selectedNote?.encrypted == true {
                        if model.selectedNote?.unlocked == true {
                            Button("Lock Encrypted Area") { model.lockEncryption() }
                        } else {
                            Button("Unlock Encrypted Area…") { model.requestEncryption(create: false) }
                        }
                    }
                    Button("New Folder  ⇧⌘N") { model.add(kind: "folder") }
                    Button("New Child Note  ⌥⌘N") { model.add(child: true) }
                    Button("New Separator  ⇧⌘=") { model.add(kind: "separator") }
                    Divider()
                    Button("Mount Notebook / Git…") { model.connectionIsRoot = false; model.showConnection = true }
                    Button("Open Root URL…") { model.connectionIsRoot = true; model.showConnection = true }
                    Button("Open Notebook…") { model.chooseFile(create: false) }
                    Button("New Notebook…") { model.chooseFile(create: true) }
                } label: { Label("New / Open", systemImage: "plus") }
                Button { model.save() } label: { Label("Save / Sync", systemImage: "arrow.triangle.2.circlepath") }
                Button { model.requestDelete() } label: { Label("Delete", systemImage: "trash") }
                    .disabled(model.selection.isEmpty)
            }
        }
        .onChange(of: model.query) { _ in model.refreshSearch() }

        .sheet(isPresented: $model.showEncryption) { EncryptionSheet(model: model) }
        .sheet(isPresented: $model.showConnection) { ConnectionSheet(model: model) }
        .confirmationDialog("Delete selected notes and their children?", isPresented: $model.confirmDelete) {
            Button("Delete", role: .destructive) { model.delete() }
        }
        .alert("FooNote", isPresented: Binding(get: { model.error != nil }, set: { if !$0 { model.error = nil } })) {
            Button("OK") { model.error = nil }
        } message: { Text(model.error ?? "") }
    }
}

struct ConnectionSheet: View {
    @ObservedObject var model: Notebook
    @State private var url = ""
    @State private var title = ""
    @State private var failure = ""
    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            Text(model.connectionIsRoot ? "Open Root URL" : "Mount Notebook / Git").font(.headline)
            TextField("/path/Notes.foonote or /path/Notes.git", text: $url)
            if !model.connectionIsRoot { TextField("Display name (optional)", text: $title) }
            Text("Git: /absolute/path/Notes.git, https://host/repo.git, or git@host:repo.git. Uses the existing Git credentials. Opening fetches; Save / Sync pushes changes.")
                .font(.caption).foregroundStyle(.secondary).fixedSize(horizontal: false, vertical: true)
            if !failure.isEmpty { Text(failure).foregroundStyle(.red).font(.caption) }
            HStack {
                Spacer()
                Button("Cancel") { model.showConnection = false }.keyboardShortcut(.cancelAction)
                Button(model.connectionIsRoot ? "Open" : "Mount") {
                    if model.connect(url: url, title: title) { model.showConnection = false }
                    else { failure = model.error ?? "Connection failed"; model.error = nil }
                }.keyboardShortcut(.defaultAction).disabled(url.trimmingCharacters(in: .whitespaces).isEmpty)
            }
        }.padding(20).frame(width: 360)
    }
}

struct EncryptionSheet: View {
    @ObservedObject var model: Notebook
    @State private var name = "Encrypted"
    @State private var password = ""
    @State private var confirmation = ""
    @State private var failure = ""
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Text(model.creatingEncryption ? "New Encrypted Area" : "Unlock Encrypted Area").font(.headline)
            if model.creatingEncryption { TextField("Name", text: $name) }
            SecureField("Password", text: $password)
            if model.creatingEncryption { SecureField("Confirm password", text: $confirmation) }
            Text("Uses the original FooNote AES encryption. Passwords are not saved; the area name remains visible.")
                .font(.caption).foregroundStyle(.secondary)
            if !failure.isEmpty { Text(failure).font(.caption).foregroundStyle(.red) }
            HStack {
                Spacer()
                Button("Cancel") { model.showEncryption = false }.keyboardShortcut(.cancelAction)
                Button(model.creatingEncryption ? "Create" : "Unlock") {
                    if model.creatingEncryption && password != confirmation {
                        failure = "Passwords do not match."; return
                    }
                    if model.unlockOrCreate(password: password, name: name) { model.showEncryption = false }
                    else { failure = model.error ?? "Unlock failed"; model.error = nil }
                    password = ""; confirmation = ""
                }.keyboardShortcut(.defaultAction).disabled(password.isEmpty)
            }
        }.padding(20).frame(width: 300)
        .onDisappear { password = ""; confirmation = "" }
    }
}

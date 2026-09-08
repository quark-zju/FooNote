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
            ContentView(model: notebook).onAppear {
                delegate.notebook = notebook
                NSApplication.shared.setActivationPolicy(.regular)
                NSApplication.shared.activate(ignoringOtherApps: true)
            }
        }
        .defaultSize(width: 340, height: 760)
        .commands {
            CommandGroup(replacing: .newItem) {
                Button("New Note") { notebook.add() }.keyboardShortcut("n")
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
        }
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
                                Button { model.reveal(hit.id) } label: {
                                    VStack(alignment: .leading, spacing: 3) {
                                        Text(model.find(hit.id)?.title ?? "Note").fontWeight(.medium)
                                        Text(hit.line).font(.caption).foregroundStyle(.secondary).lineLimit(2)
                                    }.frame(maxWidth: .infinity, alignment: .leading)
                                }.buttonStyle(.plain)
                            }
                            .overlay {
                                if model.hits.isEmpty && !model.searching { Text("No matching notes").foregroundStyle(.secondary) }
                            }
                        }
                    }
                }.frame(minHeight: 130, idealHeight: 360)
                VStack(spacing: 0) {
                    HStack {
                        Text(model.selection.count > 1 ? "\(model.selection.count) selected" : (model.selectedNote?.title ?? "Note"))
                            .lineLimit(1).font(.caption.weight(.semibold))
                        Spacer()
                        if model.selectedNote?.readOnly == true { Image(systemName: "lock") }
                    }.padding(8).background(.bar)
                    NoteEditor(model: model)
                        .overlay {
                            if model.selected == nil {
                                Text("Select a note or press ⌘N").font(.callout).foregroundStyle(.secondary)
                                    .allowsHitTesting(false)
                            }
                        }
                }.frame(minHeight: 120, idealHeight: 300)
            }
            HStack {
                Text(model.status).lineLimit(1)
                Spacer(minLength: 4)
                Text("\(model.draft.count)")
            }.font(.caption2).foregroundStyle(.secondary).padding(7)
        }
        .frame(minWidth: 260, minHeight: 380)
        .navigationSubtitle(model.location.components(separatedBy: "/").last ?? "FooNote")
        .toolbar {
            ToolbarItemGroup {
                Menu {
                    Button("New Note  ⌘N") { model.add() }
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
        .onChange(of: model.draft) { _ in model.status = "⌘S to save / sync" }
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

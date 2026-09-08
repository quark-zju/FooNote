// swift-tools-version: 5.9
import PackageDescription
import Foundation

let backend = URL(fileURLWithPath: #filePath)
    .deletingLastPathComponent().appendingPathComponent(".build/rust/debug").standardized.path
let package = Package(
    name: "FooNoteApple",
    platforms: [.macOS(.v13)],
    products: [.executable(name: "FooNoteApple", targets: ["FooNoteApple"])],
    targets: [
        .target(name: "CNoteBackend", linkerSettings: [.linkedLibrary("notebackend")]),
        .target(name: "NoteBackend", dependencies: ["CNoteBackend"]),
        .executableTarget(name: "FooNoteApple", dependencies: ["NoteBackend"]),
        .testTarget(name: "NoteBackendTests", dependencies: ["NoteBackend"])
    ]
)
// Keep the existing Rust cdylib ABI; embed an rpath for development builds.
for target in package.targets {
    target.linkerSettings = (target.linkerSettings ?? []) + [
        .unsafeFlags(["-L", backend, "-Xlinker", "-rpath", "-Xlinker", backend])
    ]
}

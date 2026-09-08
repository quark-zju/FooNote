#!/bin/sh
set -eu
cd "$(dirname "$0")"
cargo build --manifest-path ../backend/Cargo.toml -p notebackend --target-dir .build/rust
swift build
app="$PWD/dist/FooNote Apple.app"
mkdir -p "$app/Contents/MacOS" "$app/Contents/Frameworks" "$app/Contents/Resources"
cp .build/debug/FooNoteApple "$app/Contents/MacOS/FooNoteApple"
cp .build/rust/debug/libnotebackend.dylib "$app/Contents/Frameworks/"
# Rewrite Rust's build-directory install name for a self-contained app.
library_id=$(otool -D .build/rust/debug/libnotebackend.dylib | tail -n 1)
install_name_tool -change "$library_id" @rpath/libnotebackend.dylib "$app/Contents/MacOS/FooNoteApple"
install_name_tool -id @rpath/libnotebackend.dylib "$app/Contents/Frameworks/libnotebackend.dylib"
install_name_tool -delete_rpath "$PWD/.build/rust/debug" "$app/Contents/MacOS/FooNoteApple"
install_name_tool -add_rpath @executable_path/../Frameworks "$app/Contents/MacOS/FooNoteApple"
cp Info.plist "$app/Contents/Info.plist"
cp ../frontend/FooNote.app/Contents/Resources/foonote.icns "$app/Contents/Resources/"
codesign --force --deep --sign - "$app"
printf '%s\n' "Built: $app" "Run: open \"$app\""

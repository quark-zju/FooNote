import AppKit
import XCTest
@testable import FooNoteApple

@MainActor
final class EditorTests: XCTestCase {
    func testIndentUnicodeSelectionAndOutdent() async {
        let editor = EditorTextView()
        editor.string = "🐈猫\n beta\nlast"
        editor.setSelectedRange(NSRange(location: 0, length: 10)) // exclude last line
        editor.doCommand(by: #selector(NSResponder.insertTab(_:)))
        XCTAssertEqual(editor.string, "  🐈猫\n   beta\nlast")
        editor.doCommand(by: #selector(NSResponder.insertBacktab(_:)))
        XCTAssertEqual(editor.string, "🐈猫\n beta\nlast")
        XCTAssertEqual(editor.selectedRange(), NSRange(location: 0, length: 10))
    }

    func testOutdentSelectionInsideLeadingSpaces() async {
        let editor = EditorTextView()
        editor.string = "  abc"
        editor.setSelectedRange(NSRange(location: 1, length: 2))
        editor.doCommand(by: #selector(NSResponder.insertBacktab(_:)))
        XCTAssertEqual(editor.string, "abc")
        XCTAssertEqual(editor.selectedRange(), NSRange(location: 0, length: 1))
    }

    func testTabWithoutSelectionInsertsSpaces() async {
        let editor = EditorTextView()
        editor.string = "🐈x"
        editor.setSelectedRange(NSRange(location: 2, length: 0))
        editor.doCommand(by: #selector(NSResponder.insertTab(_:)))
        XCTAssertEqual(editor.string, "🐈  x")
        XCTAssertEqual(editor.selectedRange(), NSRange(location: 4, length: 0))
    }

    func testNewlineAndReadOnly() async {
        let editor = EditorTextView()
        editor.string = "    🐈"
        editor.setSelectedRange(NSRange(location: 6, length: 0))
        editor.doCommand(by: #selector(NSResponder.insertNewline(_:)))
        XCTAssertEqual(editor.string, "    🐈\n    ")
        editor.isEditable = false
        editor.setSelectedRange(NSRange(location: 0, length: 3))
        editor.doCommand(by: #selector(NSResponder.insertTab(_:)))
        XCTAssertEqual(editor.string, "    🐈\n    ")
    }
}

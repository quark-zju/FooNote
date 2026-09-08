import AppKit
import SwiftUI

/// Apply the compact default once for existing prototype users, then respect
/// subsequent manual placement rather than resetting their window every launch.
@MainActor
struct WindowPlacement: NSViewRepresentable {
    final class Anchor: NSView {
        override func viewDidMoveToWindow() {
            super.viewDidMoveToWindow()
            guard let window, !UserDefaults.standard.bool(forKey: "compactPlacementV2") else { return }
            DispatchQueue.main.async { [weak window] in
                guard let window else { return }
                let visible = (window.screen ?? NSScreen.main)?.visibleFrame ?? window.frame
                let size = NSSize(width: min(280, visible.width), height: min(760, visible.height - 24))
                window.setContentSize(size)
                let frame = window.frame
                window.setFrameOrigin(NSPoint(x: max(visible.minX, visible.maxX - frame.width - 16),
                    y: max(visible.minY, visible.maxY - frame.height - 12)))
                UserDefaults.standard.set(true, forKey: "compactPlacementV2")
            }
        }
    }
    func makeNSView(context: Context) -> Anchor { Anchor() }
    func updateNSView(_ view: Anchor, context: Context) {}
}

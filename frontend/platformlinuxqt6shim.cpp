#include <LayerShellQt/Window>

#include <QGuiApplication>
#include <QSize>
#include <QWindow>

namespace {

void configureLayerShell(LayerShellQt::Window *window, int side, int width)
{
    auto anchors = LayerShellQt::Window::Anchors{
        LayerShellQt::Window::AnchorTop,
        LayerShellQt::Window::AnchorBottom,
    };
    const auto edge = side == 1 ? LayerShellQt::Window::AnchorLeft : LayerShellQt::Window::AnchorRight;

    anchors |= edge;
    window->setScope(QStringLiteral("foonote"));
    window->setLayer(LayerShellQt::Window::LayerTop);
    window->setAnchors(anchors);
    window->setExclusiveEdge(edge);
    window->setDesiredSize(QSize(width, 0));
    window->setExclusiveZone(width);
    window->setKeyboardInteractivity(LayerShellQt::Window::KeyboardInteractivityOnDemand);
}

} // namespace

extern "C" bool foonote_layer_shell_supported()
{
    return QGuiApplication::platformName() == QStringLiteral("wayland");
}

extern "C" void *foonote_layer_shell_create(QWindow *window, int side, int width)
{
    if (window == nullptr || (side != 1 && side != 2) || width <= 0) {
        return nullptr;
    }

    auto *layerWindow = LayerShellQt::Window::get(window);
    configureLayerShell(layerWindow, side, width);
    return layerWindow;
}

extern "C" void foonote_layer_shell_configure(void *handle, int side, int width)
{
    if (handle == nullptr || (side != 1 && side != 2) || width <= 0) {
        return;
    }
    configureLayerShell(static_cast<LayerShellQt::Window *>(handle), side, width);
}

extern "C" void foonote_layer_shell_destroy(void *handle)
{
    delete static_cast<LayerShellQt::Window *>(handle);
}

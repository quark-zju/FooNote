# FooNote Apple prototype

macOS 13+ 原生 SwiftUI 前端，直接调用现有 Rust `notebackend` C ABI。
无需 Lazarus，也没有独立的数据格式或后端服务。

## 构建与运行

需要 macOS、Xcode Command Line Tools（Swift 5.9+）和 Rust/Cargo。

```sh
./apple/build.sh
open "apple/dist/FooNote Apple.app"
```

也可以在 `cargo build --manifest-path backend/Cargo.toml -p notebackend --target-dir apple/.build/rust` 后，
运行 `swift run --package-path apple`。构建脚本生成包含 Rust 动态库的本机架构
`.app`，使用本地 ad-hoc 签名；不是经过公证的分发版本。

## 原型范围

- 原生分栏界面、可展开的笔记树、纯文本编辑器。
- 新建顶层笔记、子笔记、删除节点（使用后端原有删除语义）。
- 打开和新建 `.foonote` 文件，与 LCL 前端共享格式。
- ⌘S 保存；切换笔记、打开另一文件、退出时保存。保存失败会显示错误，
  并阻止切换或退出，保留编辑内容以便重试。
- 默认数据位于 `~/Library/Application Support/FooNoteApple/Prototype.foonote`。
  第一次启动是空笔记本，不会自动打开 LCL 的数据。

本阶段只支持 macOS；SwiftUI 与 C 桥接的选择允许继续探索 iOS/iPadOS，
但这些平台尚未构建或验证。多选、拖拽、搜索、Git 同步界面、加密解锁、
停靠、撤销与 iCloud 不在本原型范围内。所有调用在主线程同步执行，
树在操作后完整加载，适合验证交互与集成，尚未针对大型笔记库优化。
没有定时自动保存；输入后使用 ⌘S，或通过上述正常切换/退出流程保存。

## 实现与验证

`Sources/CNoteBackend` 声明现有 ABI，`Sources/NoteBackend` 管理参数栈、
UTF-8 拷贝和错误处理。Rust 后端使用全局笔记本和线程局部栈，因此桥接
限定在 MainActor，应用只有一个笔记本窗口。

```sh
cargo build --manifest-path backend/Cargo.toml -p notebackend --target-dir apple/.build/rust
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer swift test --package-path apple
```

测试需要完整 Xcode（Command Line Tools 不包含 XCTest）。
集成测试使用临时笔记文件，覆盖 Unicode、层级与顺序、编辑、删除、
保存后重开，以及错误后的栈恢复。

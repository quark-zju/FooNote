# FooNote Apple prototype

macOS 13+ 原生前端，SwiftUI 外壳 + AppKit 笔记树/编辑器，直接调用现有 Rust
`notebackend` C ABI。无需 Lazarus，也没有独立的数据格式或后端服务。

## 构建与运行

需要 macOS、Xcode Command Line Tools（Swift 5.9+）和 Rust/Cargo。

```sh
./apple/build.sh
open "apple/dist/FooNote Apple.app"
```

也可以先构建 Rust，再直接启动开发版本：

```sh
cargo build --manifest-path backend/Cargo.toml -p notebackend --target-dir apple/.build/rust
swift run --package-path apple
```

脚本生成包含 Rust 动态库的本机架构 `.app`，使用本地 ad-hoc 签名，尚未公证。
如果旧原型仍在运行，请先退出，再打开新版本。

## 布局与树形编辑

默认窄条窗口（280 × 760），顶部树状笔记、底部纯文本编辑器；中间分割栏
可以上下拖动。窗口最小宽度 260。首次使用本版时会应用靠屏幕右侧的窄条布局，之后尊重手动调整的位置与尺寸。

- 新建笔记、目录、子笔记、只读分隔线条目；调用 Rust 原有 Auto-fill 延续标题序列。
- 第一行修改实时反映在树中；编辑器无重复标题栏，只读图标在底部状态栏。
- 树中右键菜单包含编辑、新建、挂载、展开/折叠及删除；右键已选项保留多选。
- 新建时选中目录/挂载节点则放入其中；选中普通条目则插到其后；无选择时放到 Root。
- ⌘点击、⇧点击多选，批量删除与拖拽；删除使用后端回收站语义。
- 拖到节点上为放入；拖到行间插入线为放在相邻条目前/后；底部空白处为追加到 Root。
- 同时选中父子节点时后端归一化为移动父节点整棵子树；禁止移入自身或后代。
- 分隔线也是可选择、移动、删除的节点，但不能编辑或放入子节点。
- 通过后端批量接口移动，跨挂载移动后使用后端返回的新 ID 恢复选择。

## 搜索与快捷键

搜索覆盖笔记正文和嵌套子树，后台搜索、输入防抖；结果显示匹配行。
点击结果会切换编辑器中的笔记，保留搜索列表与查询；编辑后即使不再匹配也不移除当前结果。Esc 可清空搜索返回树。

| 快捷键 | 行为 |
| --- | --- |
| ⌘N | 新建笔记 |
| ⇧⌘N | 新建目录 |
| ⌥⌘N | 新建子笔记 |
| ⇧⌘= | 新建分隔线 |
| ⌥⇧⌘N | 新建笔记本文件 |
| ⌘O | 打开笔记本文件 |
| ⌘S | 保存 / Git 同步 |
| ⌘F | 聚焦搜索 |
| 树中 `/` 或 Esc | 聚焦搜索 |
| 树中 Enter / 双击 | 聚焦编辑器 |
| 编辑器中 Esc | 返回树 |
| 编辑器中 Tab | 插入两空格；有选区时缩进所选各行 |
| 编辑器中 Shift-Tab | 选中各行减少最多两空格缩进 |
| 编辑器中 Enter | 新行继承当前行开头的空格缩进 |
| 搜索中 Esc | 清空搜索并返回树 |
| 搜索中 Enter / ↓ | 编辑首个结果，保留结果列表 |
| 树中 Delete | 请求删除选中节点 |

## 文件、挂载与 Git

工具栏 `+` 菜单和 File 菜单包含 **Mount Notebook / Git…** 与 **Open Root URL…**：

- 挂载：创建 `type=mount`、`mount=<URL>` 节点，子树由现有后端按需挂载。
- Root：直接使用 Git 笔记本作为根，无需先创建 `.foonote` 外壳。
- URL 支持 `/absolute/path/Notes.foonote`、`/absolute/path/Notes.git`、
  `https://host/repo.git`、`git@host:repo.git`。Git 地址使用现有后端支持的 `.git` 结尾格式。
- 本地不存在的 `.git` 路径可由后端初始化（父目录需存在）。使用专门的 FooNote 笔记仓库。
- Git 使用现有命令行 Git 与凭据；打开时 fetch，保存时 commit/push，沿用后端冲突合并逻辑。
  本前端没有单独的账户登录、凭据管理或分支管理界面。

默认首次打开 `~/Library/Application Support/FooNoteApple/Prototype.foonote`，
成功打开的 Root 地址会记住，供下次启动恢复。可用 Open Notebook 或 Open Root URL 切换。

**保存时机：** ⌘S、打开另一笔记本、正常退出时持久化。切换选中笔记、树操作会先将
编辑写入后端内存，不会每次点击都触发 Git push；最后一次修改后延迟 30 秒自动保存。
保存失败会显示错误，并阻止切换笔记本或退出，便于重试。

## 验证与限制

```sh
cargo build --manifest-path backend/Cargo.toml -p notebackend --target-dir apple/.build/rust
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer swift test --package-path apple
```

测试需要完整 Xcode（Command Line Tools 不包含 XCTest）。测试使用隔离的内存/临时文件
及本地 Git 仓库，不访问远程 Git 服务。

本阶段只支持 macOS，iOS/iPadOS 尚未适配。所有 FFI 事务固定在 MainActor，满足
Rust 线程局部参数栈要求；应用只维护一个笔记本窗口。Git 打开/同步仍为同步调用，
慢网络可能暂时阻塞界面。树整体加载，尚未针对大型笔记库优化。

尚无挂载地址编辑/卸载界面、桌面停靠、跨应用拖拽和树操作撤销。
文本编辑器支持系统撤销；切换笔记会清理撤销记录以防混入另一条笔记。


## 加密区、自动保存与恢复

通过 `+` 或树右键菜单的 New Encrypted Area 创建 AES 区域，密码需要确认。
沿用原后端 AES 文件格式，区域标题可见，内容加密；密码不保存在设置中。
双击锁定区域或右键 Unlock 可解锁，右键 Lock 重新锁定。加密区本身不可编辑，
以免修改承载密文的内容；解锁后编辑其中的子笔记。锁定时清空搜索结果和编辑缓冲。

最后一次正文或树结构修改后延迟 30 秒保存；后续修改重置计时。
保存失败会显示持续的错误区域，支持 Retry 和 Export Recovery。
导出生成系统临时目录中的唯一 `.foonote` 文件，并显示路径及 Show Recovery File；
请及时复制到长期存储位置，系统可能清理临时目录。导出不会清除原位置的保存失败状态。

恢复快照从内存生成，不依赖原文件写入或 Git push 成功：普通外部挂载展开为独立目录，
加密挂载保留最新密文和加密元数据，不展开为明文。恢复文件可通过 Open Notebook 打开。

树选择固定为整行矩形，分隔线行高为 10 点（普通行 24 点）。

.. _DevGuide:

开发者说明
==========

FooNote 是开放源代码项目，欢迎爱好者讨论修改。GitHub 地址为 
`quark-zju/FooNote <https://github.com/quark-zju/FooNote/>`_。

以下简略介绍 FooNote 源代码结构和编译运行方法。

FooNote 分为前端、后端两部分，使用不同的编程语言完成。

* 前端使用 Pascal 在 Lazarus_ 开发。LCL 库提供了跨平台桌面界面支持。
* 后端使用 Rust_ 编程语言开发。使用了 Rust 生态环境中的第三方依赖。


.. _DevGuideBackend:

后端
------

后端项目在 “backend” 目录下，目前有三个子项目。
使用 ``cargo build`` 可编译后端所有项目。使用 ``cargo test`` 运行测试。


notebackend_types
^^^^^^^^^^^^^^^^^

定义了后端笔记树相关的类型（主要是 ``trait TreeBackend``），供其他子项目使用。

其中值得一提的是这个接口：

.. code-block:: rust

    pub type CreateBackendFunc =
        fn(...) -> std::io::Result<Box<dyn TreeBackend<Id = Id>>>;

被用在了“动态链接库”后端。（Rust）动态链接库导出一个符合该接口的函数，然后在其他
（采用 Rust 的） 地方就能对导出的笔记树进行操作。这和普通的 C 函数导出只能使用非常基本的
输入输出类型有区别。


notebackend
^^^^^^^^^^^

主要的后端项目及测试，实现了：

* 一些单类型后端：FooNote 本地文件、内存、Git、AES、动态链接库。
* 混合（Multiplex）后端，支持混合挂载其他种类的后端。
* 后台增量搜索。
* 外部接口（FFI）：导出函数使 Pascal 能够调用 notebackend 中的功能。

其中的“动态链接库”提供了扩展功能，并能将一些有可选依赖的功能解耦合。


notebackend_python
^^^^^^^^^^^^^^^^^^

“动态链接库”后端的一个实现，允许使用 Python_ 编程语言来实现“笔记树”，增加了扩展性。

如果系统中没有 Python_，则这个“动态链接库”无法被加载。这也是为什么 Python_ 扩展功能
没有直接在 notebackend 中实现的原因：如果 Python_ 没有安装则会导致 notebackend
无法加载，然后 FooNote 无法使用。


.. _DevGuideFrontend:

前端
------

前端项目在 “frontend” 目录下，可以使用 Lazarus_ 打开 ``foonote.lpi`` 编辑，
或者使用 ``lazbuild`` 来直接从命令行编译。


前后端分工
^^^^^^^^^^

后端作为“数据源”（Source of truth），前端不保存和后端冗余的状态，尤其是可变
（Mutable）状态。这也是因为后端代码测试起来更容易。具体而言，

* 前端不保存每个笔记节点的全文，只保留 ID，文本编辑直接按 ID 同步到后端。
* 前端对整个笔记树“懒惰”（Lazy）处理。文件夹节点未展开之前不关心其子节点具体内容。
* 节点移动操作通过后端 API 完成，尽管 LCL 提供了 ``TTreeNode.MoveTo`` 方法，
  但前端不使用。后端移动操作完成后，前端使用 ``RefreshFullTree`` 对整个笔记树
  进行增量刷新。有一些类似 React.js 的思路。


单元
^^^^

``NoteBackend`` 单元提供了调用后端接口的方法和函数。

``TreeNodeData`` 单元定义了匹配后端的笔记节点的数据模型。每一个可见的笔记节点都有一个对应的
``TTreeNodeData``，``TTreeNodeData`` 提供节点的文本、图标等信息（Source of truth），
而不是 LCL ``TTreeView`` 控件的 ``TTreeNode``。``TreeViewSync`` 单元提供了将
``TTreeNodeData`` 同步到 ``TTreeNode`` 并在界面上展示的方法，并也提供了按需更新子节点
的方法。由于是按需更新，前端可以展示递归循环的挂载点。

``PlatformWindows`` 单元提供了依赖 Windows 系统的一些功能，如桌面停靠等。

``SciEdit`` 单元提供了对 Scintilla_ 控件的简单包装。目前主要是支持 Windows 版。

其他单元基本上是界面相关的具体业务逻辑。


翻译
^^^^^^

FooNote 支持中英双语。后端代码中使用 ``t!`` 宏直接嵌入双语文本。前端代码使用英文，嵌入了
中文翻译文件。

如果在前端新增了界面上的字符串，要更新嵌入的中文翻译，首先用 Poedit_ 编辑
``frontend/locale/foonote.cn.po``，然后运行 ``frontend/locale/update.bat``
更新 ``cn.lrs`` 文件。``cn.lrs`` 会被嵌入到前端代码中使新翻译生效。


第三方项目
----------

第三方项目及 FooNote 对其的定制在 “thirdparty” 目录下。


图标
^^^^

前端界面使用 Elementary_ 项目的一些图标。``extract.py`` 脚本能将各种尺寸的图标统一提取。
高尺寸图标用于高 DPI。图标已经保存在前端的资源文件中，无需重新提取。


Scintilla
^^^^^^^^^

前端笔记编辑区使用了 Scintilla_ 编辑器。具体编译方法详见其 ``README`` 文件。
FooNote 做了少量修改，对右键菜单添加了中英双语支持。
Scintilla_ 为可选组件，如果不存在则 FooNote 会使用系统原生文本编辑控件做替代。


运行
------

前端运行需要加载后端的动态链接库。简单的办法是：

* Windows 下：将后端编译出的 DLL 复制到前端 exe 文件的同一目录下
* Linux 下：设置 ``LD_LIBRARY_PATH`` 环境变量使其包含后端 ``so`` 文件


调试日志
^^^^^^^^

有时需要查看运行时的调试日志以方便理解 FooNote 到底做了什么。

FooNote 使用 Rust 社区的 env_logger_。通过设置 ``FOONOTE_LOG`` 来调整日志输出级别。
例如，``FOONOTE_LOG=trace`` 会输出所有调试日志。``FOONOTE_LOG=warn,frontend=info``
输出前端的信息级别日志，非前端警告级别日志。如果 ``FOONOTE_LOG`` 未设置，则不输出日志。


.. _DevGuideFileFormat:

文件格式
--------

笔记树抽象格式
^^^^^^^^^^^^^^

FooNote 的笔记树对每个笔记节点分配了 ID，抽象的笔记树包含以下内容：

* 父子节点关系，即 ``{父节点ID: [子节点ID]}`` 映射。
* 节点元数据，即 ``{节点ID：元数据字符串}`` 映射。元数据包含了节点是否为挂载点、分隔栏、文件夹等信息。
* 下一个未分配的子节点 ID。
* 节点文本，即 ``{节点ID：文本}`` 映射。


本地文件格式
^^^^^^^^^^^^

对于本地文件后端，将抽象信息放在一起通过 serde_ 进行 JSON 序列化，保存到本地文件。

早期未发布版本有考虑过更为紧凑的 varbincode 格式，但后续基于兼容性和方便使用其他工具读取的
考虑使用了 JSON。将来如果文件体积是一个问题的话也可以考虑使用 CBOR 格式，再套用某种压缩算法。


内存笔记树格式
^^^^^^^^^^^^^^

内存笔记树用于复制粘贴，以及 AES 加密后端解密后的格式。类似于本地文件，通过 serde_ 对抽象
数据序列化，但序列化采用更为紧凑的 CBOR 格式。


.. _DevGuideFileFormatGit:

Git 后端格式
^^^^^^^^^^^^

Git 后端和本地文件后端稍有不同。每个节点的文本单独保存在 “notes” 文件夹下，以节点 ID 为
名的文件中，这样做使得 Git 更能容易实现增量压缩，也使得像 ``blame`` 这样的 Git 操作更有
意义，还可以让 FooNote 在加载 Git 仓库时能够不用读取所有节点的文本内容。

对于父子关系，元数据等其他信息，Git 后端也采用 JSON 序列化，保存在 ``manifest.json``
文件中。

为了最好的兼容性（比如兼容 ssh 配置），以及避免在编译过程中需要对 OpenSSL 的依赖
（有时会比较难配置）。FooNote 使用系统 Git_ 程序，不采用 libgit2_。

为了支持远程的 Git 仓库，FooNote 会在系统缓存文件夹中创建本地副本，使用 ``git fetch``
和 ``git push`` 来和远程仓库同步。发生冲突时，使用内在的冲突解决机制强制解决冲突。解决
的冲突会产生一个合并提交（Merge Commit），在自动冲突解决有误时可以手动查看冲突双方来手动
修正。为了处理上的方便，对于本地的 Git 仓库，FooNote 也将其当作远程仓库看待，仍使用系统
缓存文件夹对其进行操作。

FooNote 操作 Git 仓库时不使用工作副本（Working Copy）。读取 Git 对象使用
``git cat-file --batch``，写入 Git 时使用 ``git fastimport``，这使得 FooNote
在即便未使用 libgit2_ 的情况下也能有较好的性能。


.. _DevGuideFileFormatAES:

AES 加密数据格式 
^^^^^^^^^^^^^^^^

AES 加密后端使用 AES-GCM-SIV 加密算法，使用 256 比特的密钥，以及 scrypt_ 密钥派生函数。
在笔记文本中以 base64 形式显示，其二进制内容为：

.. list-table::

    * - 32 字节
      - 盐
      - 创建加密节点时随机生成，用于 scrypt_
    * - 12 字节
      - IV
      - 创建时随机生成，保存时随机改变。
    * - 不限长度
      - 密文
      - 原文为内存后端的 CBOR 序列化


额外信息
^^^^^^^^

除了笔记树本身的信息，还有一些额外信息，如最后一次选定的节点。这个信息只影响前端如何“展现”
后端数据，不影响后端数据本身，所以保存在 FooNote 前端配置文件中。前端配置文件的位置可在
笔记树区域右键菜单选择“关于”来查看。不同的根节点地址使用不同的前端配置文件。


文档
------

FooNote 文档在 “doc” 目录下，是一个 Sphinx_ 项目。


.. _DevGuideLangChoice:

关于编程语言的选择
------------------

编程语言众口难调，生态环境也不同。新兴的 Rust_ 语言避免了很多历史包袱，
很多时候能带来“编译通过即正确”的体验，也有丰富的第三方库，是优先选择。

桌面图形界面十分复杂，没有看上去简单。好的图形界面有很多不是那么明显的功能，
比如输入法、触摸屏、多屏幕、高 DPI、高对比度主题，等等。图形界面库是主要考虑因素，
不能只考虑编程语言。绝大多数不是很主流的图形界面库（包括 Rust 社区的许多新兴项目）
连右键弹出菜单，或者是支持输入法的多行文本框都没有，不能采用。
较为成熟的跨平台主流图形界面库只有 Qt、Gtk、 wxWidgets、LCL。其中，LCL 编译最方便，
编译结果体积最小，功能较丰富，开发较方便，故采用。

基于浏览器技术的 PWA，Electron，DeskGap，Webview 等也有考虑。FooNote 需要一些原生功能
（如置顶，Windows 桌面停靠），这就否决了 PWA。原生桌面控件中有树形控件、菜单等，附带有
高 DPI、高对比度、快捷键支持等。全使用 JavaScript 重新实现并不方便。另外还有体积上的考虑，
权衡后，没有采用浏览器技术。

非跨平台的桌面界面技术（如 Rust 社区的 ``native-window-gui``）也有考虑，
优点是程序体积会小，编程语言可以是 Rust。尽管 FooNote 看起来好像不需要多少功能，
但把 i18n、自定义绘制“分隔栏”、基于锚点的自动布局等全算上仍有很多工作量。
雪上加霜的是原生控件还需要修修补补才能用。比如 Windows 下 ``comctl32.dll`` 的原生
树形控件好像是有多选功能（``TVS_EX_MULTISELECT``），但实际用一下会发现好像是半成品，
MSDN 也说“不支持，不要用”。必须要手动实现多选功能。相比之下直接使用 Lazarus_ 会方便太多。


.. _Lazarus: https://www.lazarus-ide.org/
.. _Rust: https://www.rust-lang.org/
.. _Scintilla: https://www.scintilla.org/
.. _Poedit: https://poedit.net/
.. _Sphinx: https://www.sphinx-doc.org/
.. _env_logger: https://crates.io/crates/env_logger
.. _serde: https://serde.rs/
.. _scrypt: https://tools.ietf.org/html/rfc7914
.. _libgit2: https://libgit2.org/
.. _Git: https://git-scm.com/
.. _Python: https://www.python.org/
.. _Elementary: https://elementary.io/
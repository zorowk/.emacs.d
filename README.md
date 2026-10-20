# Emacs Configuration

面向日常编程、Org/Denote 笔记和阅读写作的个人 Emacs 配置，最低支持 Emacs 31.0.90。
使用 Emacs 内置的 `package.el` 和 `use-package` 管理第三方包。

本配置源自 [M-EMACS](https://github.com/MatthewZMD/.emacs.d)，保留 Mingde
(Matthew) Zeng 的原始版权；后续修改由 zorowk 维护并署名。具体归属以各源文件头为准。

## 功能概览

- 补全：Corfu、Cape、Vertico、Orderless、Consult、Embark、Marginalia
- 开发：Eglot、内置 Tree-sitter、AUCTeX、Magit
- 编辑：内置 Electric Pair、Tempo 与 Outline、Expreg、Avy、Vundo
- 笔记：Org、Denote、Consult Denote；标题使用原生星号并隐藏前导星号
- 阅读与网络：EWW、Elfeed、Nov、Elpher、ERC、Gnus
- 界面：Ef Themes、Spacious Padding、Pulsar、Popper、Olivetti、Dashboard，以及紧凑的 mode-line
- AI：Agent Shell

## 配置结构

| 文件 | 职责 |
| --- | --- |
| `early-init.el` | 版本检查、包激活策略和基础界面设置 |
| `init.el` | 建立加载路径并按职责加载各模块 |
| `elisp/init-const.el` | 个人身份和跨模块共享目录 |
| `elisp/init-core.el` | 全局行为、按键、编码和无持久状态的 Hook |
| `elisp/init-files.el` | Recentf、备份、自动保存、Customize 和文件模式 |
| `elisp/init-ui.el` | 启动主题、字体、窗口和基础显示行为 |
| `elisp/init-search.el` | minibuffer 补全、搜索和 Embark |
| `elisp/init-complete.el` | buffer 内补全、Cape 与 Eglot |
| `elisp/init-templates.el` | 内置 Tempo 模板、按 mode 注册和展开命令 |
| `elisp/init-*.el` | 其余按功能拆分的独立模块 |
| `elisp/init-package.el` | 内置 package.el、软件源优先级和 use-package 设置 |

## 安装

```sh
git clone https://github.com/zorowk/.emacs.d.git ~/.emacs.d
emacs
```

第一次启动需要网络连接。带有 `:ensure t` 的第三方包会优先从 GNU ELPA、NonGNU
ELPA 安装，并以 MELPA 作为补充来源。内置包统一标记为 `:ensure nil`，同时禁止
package.el 用软件源版本替换 Emacs 自带库。

个人身份、邮箱地址和 Dropbox 路径集中在 `elisp/init-const.el`。如果账号或目录布局
不同，只需修改该文件中的共享常量。

内置模板使用 `M-+` 展开光标前的标签，或使用 `M-*` 从当前 major mode 可用的模板中
选择。模板插入后可通过 `C-c t n` 跳到下一个占位位置。

Org 的 PDF 导出使用 XeLaTeX。编译后由 Org 清理辅助文件，保留 `.tex`、`.log`、
`.blg` 等排错材料及 `auto/` 目录。

Org/Agent Shell 任务和代码上下文集成是可选功能，脚本来自
`${CODEX_HOME:-~/.codex}/skills/` 下的 `emacs-gtd-assistant` 和
`emacs-code-navigator` 技能目录。未安装技能时跳过相应集成，基础配置仍可启动。

## 更新所有包

1. 运行 `M-x package-refresh-contents` 刷新软件源。
2. 运行 `M-x package-upgrade-all` 升级 package.el 管理的包。
3. 重启 Emacs，并执行下面的批处理启动检查。

配置以明确的 `:ensure`/`:vc` 声明、软件源优先级和 Emacs 31 内置库优先策略保持
包管理路径单一。

## 验证

从仓库根目录执行一次独立启动检查：

```sh
emacs --batch -Q -l early-init.el --eval '(package-activate-all)' -l init.el
```

`--batch -Q` 不会自动激活第三方包，因此命令在 `early-init.el` 和 `init.el` 之间显式模拟
正常 GUI/daemon 启动的 package 激活阶段。启动不应产生 Elisp 错误或过时警告。ERC、Gnus、
Org 随任务入口加载；Denote 等其余较重模块按需加载，不应仅因启动配置而提前进入
`features`。

需要检查同步启动关键路径时，运行模块计时报告：

```sh
emacs --batch -Q -l early-init.el -l elisp/benchmark-startup.el \
  --eval '(zoro-startup-benchmark-activate-packages)' -l init.el \
  --eval '(zoro-startup-benchmark-report)'
```

报告包含 package 激活、每个 `init-*` 模块的首次加载、累计耗时和 GC 次数。
批处理检查不绘制 Dashboard，也不导入图形会话的登录 shell 环境，因此不代表 GUI 首屏时间。

交互启动时同步绘制 Dashboard；图形会话在 Linux 和 macOS 上异步导入登录 shell 环境。
任务技能集成在首次进入 Org、加载 Agenda 或 Agent Shell 时加载；代码上下文集成在
Agent Shell 加载后启用。普通启动不加载 Org、Agenda 或技能运行库。

### 自动测试

在已经安装配置所声明包的环境中运行完整 ERT：

```sh
emacs --batch -Q -L test -l test/run-tests.el
```

测试加载完整配置，覆盖配置自有函数、包来源、内置包保护，以及 Eglot、Cape、Corfu、
Vertico、Orderless 等补全组件的职责边界。已确认但尚未修复的问题记录在
`test/FINDINGS.md`，并可暂时表示为 ERT expected failure；unexpected failure 始终使测试失败。

依赖本机可选技能的集成测试在对应脚本缺失时标记为 skipped；脚本存在但集成失效时仍会失败。
CI 只验证仓库内的基础配置，不要求安装个人技能。可在本机用同一命令验证完整集成，或用
`CODEX_HOME=/tmp/zoro-no-skills` 前缀验证缺少技能时的基础配置（该目录应不存在）。

运行 Emacs 31.0.90 兼容性编译和多次 warm startup 测量：

```sh
emacs --batch -Q -l scripts/byte-compile-config.el
scripts/run-startup-benchmark.sh
```

兼容性编译将警告视为错误，并把 `.elc` 写入临时目录。性能脚本默认运行 5 次，报告中位数、
最慢一次和 2000ms 安全上限；可通过 `ZORO_STARTUP_RUNS` 与
`ZORO_STARTUP_MAX_MS` 调整。

GitHub Actions 同时验证最低基线 `release-snapshot`（不低于 31.0.90）和最新开发快照。
最低基线为必过任务，最新快照暂为前瞻性允许失败任务。每次运行都把九维、满分 100 的证据
评分写入 Job Summary；评分规则位于 `scripts/config-score.el`，不会隐藏在 workflow 中。

运行时产生的 `agent/`、`elpa/`、`projects` 和其他历史/缓存文件均被 Git 忽略；仓库只
保存手写配置，不提交下载后的包或构建产物。

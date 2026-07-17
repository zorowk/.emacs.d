# Emacs Configuration

面向日常编程、Org/Denote 笔记和阅读写作的个人 Emacs 配置。当前跟随 Emacs master，
使用 `straight.el` 管理第三方包，并提交版本锁文件以保持环境可复现。

## 功能概览

- 补全：Corfu、Cape、Vertico、Orderless、Consult、Embark、Marginalia
- 开发：Eglot、内置 Tree-sitter、ESS、AUCTeX、Magit
- 编辑：内置 Electric Pair、Expreg、Avy、Ace Window、Vundo
- 笔记：Org、Denote、Consult Denote、Org Bullets
- 阅读与网络：EWW、Elfeed、Nov、Elpher、ERC、Gnus
- 界面：Ef Themes、Spacious Padding、Pulsar、Popper、Olivetti、Dashboard
- AI：Agent Shell

## 配置结构

| 文件 | 职责 |
| --- | --- |
| `early-init.el` | 启动阶段的 GC、文件处理器和基础界面设置 |
| `init.el` | 建立加载路径并按职责加载各模块 |
| `elisp/init-const.el` | 个人身份、平台判断和共享外部数据目录 |
| `elisp/init-core.el` | 全局行为、按键、编码和无持久状态的 Hook |
| `elisp/init-files.el` | Recentf、备份、自动保存、Customize 和文件模式 |
| `elisp/init-search.el` | minibuffer 补全、搜索和 Embark |
| `elisp/init-complete.el` | buffer 内补全、Cape 与 Eglot |
| `elisp/init-*.el` | 其余按功能拆分的独立模块 |
| `straight/versions/default.el` | 第三方包及配方仓库的版本锁文件 |

`init-private.el` 如果存在，会在所有普通模块之后加载。它已被 Git 忽略，适合存放不应
提交的本机配置；密码和令牌应继续使用 `auth-source`，不要写进配置文件。

## 安装

```sh
git clone https://github.com/zorowk/.emacs.d.git ~/.emacs.d
emacs
```

第一次启动需要网络连接来引导 Straight 并克隆缺失的包。仓库中的
`straight/versions/default.el` 会让新环境使用已经验证过的包版本。

个人 Dropbox 路径集中在 `elisp/init-const.el`。如果目录布局不同，只需修改该文件中的
共享常量。

## 更新所有包

Straight 可以直接升级全部包：

1. 运行 `M-x straight-pull-all` 拉取并合并所有包的上游更新。
2. 重启 Emacs，或在仓库根目录运行下面的批处理启动检查。
3. 确认配置正常后，运行 `M-x straight-freeze-versions` 更新版本锁文件。
4. 检查并提交 `straight/versions/default.el` 的变化。

Straight 会在需要时重建发生变化的包。只有排查构建问题时，才需要手动运行
`M-x straight-rebuild-all` 强制重建全部包。

要恢复锁文件记录的版本，运行 `M-x straight-thaw-versions`。该命令遇到包仓库中的本地
改动时会交互确认；升级前不要把个人修改遗留在 `straight/repos/` 中。

## 验证

从仓库根目录执行一次独立启动检查：

```sh
emacs --batch -Q -l early-init.el -l init.el
```

正常启动不应产生 Elisp 错误或过时警告。ERC、Gnus、Org、Denote、Hyperbole 等较重模块
按需加载，不应仅因启动配置而提前进入 `features`。

运行时产生的 `agent/`、`elpa/`、`projects`、Straight 构建目录和其他历史/缓存文件均被
Git 忽略；可复现状态由手写配置和 Straight 锁文件共同定义。

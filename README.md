# Emacs Configuration

面向日常编程、Org/Denote 笔记和阅读写作的个人 Emacs 配置，最低支持 Emacs 31。
使用 Emacs 内置的 `package.el` 和 `use-package` 管理第三方包。

## 功能概览

- 补全：Corfu、Cape、Vertico、Orderless、Consult、Embark、Marginalia
- 开发：Eglot、内置 Tree-sitter、ESS、AUCTeX、Magit
- 编辑：内置 Electric Pair 与 Tempo、Expreg、Avy、Ace Window、Vundo
- 笔记：Org、Denote、Consult Denote、Org Bullets
- 阅读与网络：EWW、Elfeed、Nov、Elpher、ERC、Gnus
- 界面：Ef Themes、Spacious Padding、Pulsar、Popper、Olivetti、Dashboard
- AI：Agent Shell

## 配置结构

| 文件 | 职责 |
| --- | --- |
| `early-init.el` | 版本检查、包激活策略和基础界面设置 |
| `init.el` | 建立加载路径并按职责加载各模块 |
| `elisp/init-const.el` | 个人身份、平台判断和共享外部数据目录 |
| `elisp/init-core.el` | 全局行为、按键、编码和无持久状态的 Hook |
| `elisp/init-files.el` | Recentf、备份、自动保存、Customize 和文件模式 |
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

当前配置没有 Git-only 包。以后若需直接跟踪 Git 仓库，应使用 Emacs 31 内置的
`use-package :vc`，由 `package-vc` 安装，而不是引入第二套包管理器。

个人身份、邮箱地址和 Dropbox 路径集中在 `elisp/init-const.el`。如果账号或目录布局
不同，只需修改该文件中的共享常量。

内置模板使用 `M-+` 展开光标前的标签，或使用 `M-*` 从当前 major mode 可用的模板中
选择。模板插入后可通过 `C-c t n` 跳到下一个占位位置。

## 更新所有包

1. 运行 `M-x package-refresh-contents` 刷新软件源。
2. 运行 `M-x package-upgrade-all` 升级 package.el 管理的包。
3. 如果以后加入 `:vc` 包，再运行 `M-x package-vc-upgrade-all` 更新 Git 包。
4. 重启 Emacs，并执行下面的批处理启动检查。

配置以明确的 `:ensure`/`:vc` 声明、软件源优先级和 Emacs 31 内置库优先策略保持
包管理路径单一。

## 验证

从仓库根目录执行一次独立启动检查：

```sh
emacs --batch -Q -l early-init.el -l init.el
```

正常启动不应产生 Elisp 错误或过时警告。ERC、Gnus、Org、Denote、Hyperbole 等较重模块
按需加载，不应仅因启动配置而提前进入 `features`。

运行时产生的 `agent/`、`elpa/`、`projects` 和其他历史/缓存文件均被 Git 忽略；仓库只
保存手写配置，不提交下载后的包或构建产物。

# Emacs Configuration

轻量、高效、现代化的 Emacs 配置，专注 C++ 开发 + Org-mode 笔记 + 阅读写作。

当前基于 **Emacs 31**，使用 **straight.el** 管理包，极简但功能强大。

## 核心特点

- **补全体系**：Corfu + Cape + Vertico + Orderless + Consult + Embark + Marginalia（极致补全体验）
- **LSP & 高亮**：Eglot + clangd + treesit-auto（C++23/内核代码高亮 & 语义跳转）
- **项目管理**：Projectile + Consult-Projectile（快速搜索、打开项目文件）
- **Org & 写作**：Org-roam / Denote + Tempel + Org-modern（知识管理 & 模板）
- **美化 & 留白**：Spacious-padding + Ef-themes / Modus-themes（呼吸感界面）
- **其他**：Magit（Git）、Smartparens（括号）、Iedit（批量编辑）、Format-all（自动格式化）

## 主要包列表（精选）

| 分类           | 包名                              | 作用简述                              |
|----------------|-----------------------------------|---------------------------------------|
| 补全           | corfu / cape / vertico / orderless / consult / embark / marginalia | 现代补全栈（buffer / minibuffer）     |
| LSP & 高亮     | eglot / treesit-auto / clangd     | C++ 语义补全 & Tree-sitter 高亮       |
| 项目管理       | projectile / consult-projectile   | 项目文件搜索 & 切换                   |
| 笔记 / 知识管理| org-roam / denote                 | 原子化笔记 / 知识网络                 |
| 模板           | tempel                            | 轻量代码/文本模板                     |
| Git            | magit                             | Git 一站式管理                        |
| 美化           | spacious-padding / ef-themes      | 全局留白 + 现代主题                   |
| 编辑增强       | smartparens / iedit               | 智能括号 / 多光标批量编辑             |
| 格式化         | format-all                        | 多语言自动格式化                      |
| 阅读           | nov + shrface                     | EPUB 阅读 + 美化                      |
| 其他           | vundo / avy / ace-window          | 可视化 undo / 快速跳转                |

完整列表见仓库根目录下的 `straight/repos` 或 `.emacs.d/elpa`。

## 安装与使用

1. **克隆仓库**
   ```bash
   git clone https://github.com/zorowk/.emacs.d.git ~/.emacs.d

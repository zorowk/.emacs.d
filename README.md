- [M-EMACS](#orgeb050fe)
- [Why EMACS](#org0f18c1d)
- [Why M-EMACS](#orgb441d3e)
  - [Community Responses ❤️](#org6bd6b4d)
  - [About README](#org2358066)
  - [Installation](#org210249f)
  - [Modification](#org0360179)
  - [Contribution](#orgf7750f8)
  - [Special Thanks](#org9ef0b91)
- [Startup](#orgfd1bec8)
  - [Lexical Binding](#org98c2aae)
  - [Early Init](#org146f8ce)
    - [Compatibility With 26](#org181556c)
    - [Defer Garbage Collection](#org8abbbba)
    - [Disable `package-enable-at-startup`](#org3890555)
    - [Unset `file-name-handler-alist`](#orgebf6495)
    - [Disable `site-run-file`](#orgb62feb5)
    - [Disable Unnecessary Interface](#org357eb8a)
  - [Garbage Collection](#org125bd13)
    - [Adjust `gc-cons-threshold` for Interactive Use](#org89d2d11)
  - [Load Path](#org5fc0c29)
  - [Define Constants](#org05703f6)
  - [Load Private File](#org77f8e36)
- [Package Management](#org2044298)
  - [Straight](#orgdc3eee0)
  - [Use Package](#org3c64b1e)
  - [Diminish](#org37a148e)
- [Global Functionalities](#orgef1d046)
  - [User Information](#orge640cc4)
  - [Bindings](#orgf057944)
  - [Avy](#orgc54599e)
  - [Crux](#org767e64d)
  - [Ivy, Amx, Counsel, Swiper](#org179d423)
  - [Color Ripgrep](#orgaab6f97)
  - [Find File In Project](#org0975333)
  - [Files Directories](#org8d5e9e9)
    - [Dired](#org7fe3a22)
    - [Disk Usage](#org3a7445c)
    - [Save All Buffers](#orgad53fd9)
  - [Winner](#org01756db)
  - [Which Key](#org0b213ee)
  - [Undo Tree](#org869a304)
  - [Discover My Major](#orgb1bb92b)
  - [Ace Window](#org659b5b1)
  - [Terminal](#org7b861c9)
    - [Vterm](#org9ef1923)
    - [Shell Here](#org966940d)
    - [Multi Term](#org3f055f3)
    - [Term Keys](#org21a4e4c)
    - [Exec Path From Shell](#orgce1e8d8)
  - [Sudo Edit](#orge64ac27)
  - [Ibuffer](#org00f5682)
  - [Config](#orgcbd3dd0)
    - [UTF-8 Coding System](#org747536f)
    - [Optimize Editing Experience](#orge01e63f)
    - [History](#orgad3167d)
    - [Small Configs](#orgb28783b)
  - [Functions](#org4df23aa)
    - [Resize Window Width / Height Functions](#org8df0b34)
    - [Edit This Configuration File Shortcut](#orgccfd0a9)
    - [Update Org Mode Include Automatically](#orgb22e2ae)
    - [MiniBuffer Functions](#orgcd7ee87)
    - [Display Line Overlay](#org66708a4)
    - [Read Lines From File](#orge44f1a2)
    - [Where Am I](#org2229b8c)
- [UI Enhancements](#orgb86987f)
  - [Doom Themes](#org634c3cb)
  - [Doom Modeline](#org511f2d0)
  - [Dashboard](#org9296872)
    - [Dashboard](#org9645cb7)
    - [Page Break Lines](#org0a106c0)
  - [Fonts and Icons](#orgd2c485a)
    - [Fonts](#orga705052)
    - [All The Icons](#orge3f0e94)
  - [Smooth Scrolling](#org0cfdddd)
  - [Highlight Lines](#org18d86e8)
  - [Prettify Symbols](#org20e81a9)
  - [UI Configs](#org58c907e)
    - [Title Bar](#orged05310)
    - [Simplify Yes/No Prompts](#org26835ec)
    - [Disable Splash Screen](#org2b9062c)
    - [Line Numbers](#orgaf76dd1)
    - [Modeline Time and Battery](#orgdab350b)
    - [Pixel Scroll Precision Mode](#orgb54bf7a)
- [General Programming](#org5eb09a6)
  - [Aidermacs](#org3670e66)
  - [Magit](#org8334fce)
  - [Projectile](#org011135e)
  - [YASnippet](#org0241b96)
    - [YASnippet](#orgf0863f6)
  - [Treesit Parser Manager](#orgecaed7a)
  - [Dumb Jump](#org8ef21a8)
  - [Parenthesis](#org2b8330f)
    - [Smartparens](#orgaf22c8a)
    - [Match Parenthesis](#org70a5f7b)
  - [Indentation](#orge9ba2a4)
  - [Format All](#org32752b2)
  - [Ediff](#orgd6afb6c)
  - [Package Lint](#org84cc902)
  - [Evil Nerd Commenter](#orgba2397c)
  - [Editing](#org7344cdd)
    - [Iedit](#orgd479f71)
    - [Delete Block](#orgea2f029)
  - [Headers](#org30d29e7)
  - [Jupyter Notebook](#org669b6f2)
    - [Usage](#org0e446fe)
  - [Completion / LSP](#org3696d23)
- [Programming](#org9700c02)
  - [C/C++/Objective C](#org7ea7e6f)
    - [CCLS](#org8e4fa95)
    - [Modern C++ Font Lock](#org9dfb98d)
  - [Golang](#org2470c2f)
  - [Rust](#org22b3735)
  - [Python](#org3b3826f)
  - [ESS](#org7bb6fcd)
  - [TeX](#org72dcef4)
    - [AUCTeX](#org8170b8f)
  - [Yaml](#orgfcf1481)
    - [Yaml-Pro](#org53ebf28)
  - [Buildsystem](#org81d7913)
    - [Docker](#org4dc68a0)
    - [Groovy](#org9ec277f)
    - [Cmake](#org2c434f6)
    - [Bazel](#org0e02435)
- [Web Development](#orgdefd555)
  - [Web](#org780ca11)
  - [JavaScript/TypeScript](#org24120a2)
    - [JavaScript2](#org2715a4b)
    - [TypeScript](#org2bba325)
    - [Vue](#orga69979e)
  - [Emmet](#org477847a)
  - [Instant Rename Tag](#org20cc77f)
  - [JSON](#org81ce792)
- [Office](#org868fbad)
  - [Org](#org07377a4)
    - [Org Roam](#orgbdc3c27)
    - [HTMLize](#org337cfee)
    - [GFM Exporter](#org08c49a3)
    - [PlantUML and Graphviz](#org44dbc9b)
- [Multimedia](#org36de13a)
  - [EAF](#org448ca4d)
- [Internet](#orgd10f5d0)
  - [ERC](#org4a9e6bf)
  - [MU4E](#orgb134c85)
  - [Tramp](#org7c41755)
    - [Google Cloud Platform](#org5b4e176)
  - [LeetCode](#org481ba4f)
  - [Debbugs](#orga6bed7a)
  - [Hacker News](#org119f12d)
  - [EWW](#orgf7fdcaa)
- [Miscellaneous](#org937c86a)
  - [Chinese](#orgc58364d)
    - [Pyim](#org8e5ba12)
    - [Youdao](#orgaf32515)



<a id="orgeb050fe"></a>

# M-EMACS


<a id="org0f18c1d"></a>

# Why EMACS

Emacs transforms your approach to programming.

Emacs is **entirely introspectable**, allowing you to easily discover, "What code executes when I press this button?" This level of insight promotes an understanding of your work and deepens your engagement with the code.

Emacs serves as an **incremental programming environment**. You can avoid the traditional edit-compile-run cycle, which often interrupts workflow. Instead, you can write and execute small snippets of code, gradually developing them into a complete project without the need to switch contexts. The lines between your editor and interpreter blur seamlessly.

Emacs offers a **mutable environment**. You can modify variables, adjust functions with advice, or even redefine entire functions on the fly. This flexibility ensures that everything is open for customization, empowering you to create an environment tailored to your needs.

Emacs delivers **integrated functionality without the need for applications**. Instead of relying on disparate applications, all features are cohesively bundled within your Emacs instance. This means you can leverage the same snippet tool for writing C++ classes or crafting emails, enhancing efficiency and coherence in your tasks.

Emacs is rich with **innovative software concepts that have yet to gain mainstream traction**. Highlights include:

-   While most platforms are limited to a single-item clipboard, Emacs boasts an **infinite clipboard**, allowing for more fluid copying and pasting.
-   If you undo a change and then keep editing, many applications restrict you from redoing the original change. In contrast, Emacs enables **undoing to any historical state**, supporting a tree-based exploration of your editing history.
-   With Emacs, you can perform a **reverse variable search**, making it possible to find variables set to a specific value.
-   It facilitates **structural editing** of code, enabling you to make changes without breaking the syntax, effective for both Lisp (using paredit) and non-Lisp languages (using smartparens).
-   Many applications employ a modal GUI where certain tasks block other edits, such as during a find-and-replace operation. Emacs, however, provides **recursive editing**, allowing you to pause your current task, perform other edits, and then return to where you left off.

Emacs fosters a **rich documentation culture**. It includes an extensive usage manual, a Lisp programming manual, in-depth docstrings, and even an interactive tutorial, ensuring that help is always readily available.

Emacs also boasts a **broad ecosystem**. Whatever niche programming language you wish to work with, there’s likely an Emacs package available for it, enhancing its versatility.

While Emacs certainly isn’t the only tool with valuable features, we believe that the [Emacs learning curve](https://i.stack.imgur.com/7Cu9Z.jpg) is well worth the investment.

*This section was based on [Remacs](https://github.com/remacs/remacs).*


<a id="orgb441d3e"></a>

# Why M-EMACS

M-EMACS is a customized GNU Emacs setup designed to enhance your experience while providing an easily navigable resource. Our detailed README includes nearly the entire configuration code, making it a valuable reference for users.

I remember the challenges of finding a clear and well-organized configuration when I first started using Emacs. Often, source code comments can be hard to notice or insufficiently detailed. That's why I've created this README to offer clear, human-friendly explanations. This guide is perfect for beginners who are unsure where to start with their personal configuration. Feel free to explore this document and copy any part of it for your own use.

This distribution is specifically designed and tested for GNU Emacs 26.1 and higher. However, we recommend using Emacs 29, the latest stable version, due to its significant core improvements that enhance the overall experience beyond M-EMACS. ![img](images/Sample.png)


<a id="org6bd6b4d"></a>

## Community Responses ❤️

Some heartwarming responses from the Emacs community:

-   *"Actually I understated how much I liked reading through your config&#x2026; What makes me excited about this config is the readability and possibility of extending in a similar way."* &#x2013; from [u/Orgmonics](https://www.reddit.com/r/emacs/comments/eewwyh/officially_introducing_memacs/fc5x1lz?utm_source=share&utm_medium=web2x&context=3)
-   *"I have to say Matt's setup has the best clarity of all emacs setups I have ever tried. It's really a good template to develop your own emacs config. Thanks again&#x2026;"* &#x2013; from [u/fqye](https://www.reddit.com/r/emacs/comments/eewwyh/officially_introducing_memacs/fbxk831?utm_source=share&utm_medium=web2x&context=3)
-   *"Thanks for the fantastic emacs setup, I love emacs, but trying to get lsp working right was killing me, yours worked out of the box and all I had to do was add some bindings, it's really a time saver"* &#x2013; from [ahonnecke](https://github.com/MatthewZMD/.emacs.d/issues/48#issuecomment-877827124)
-   *"Thank you for helping a guy out and for sharing this. I hope this evolves to be into something really big."* &#x2013; from [d3v-S](https://github.com/MatthewZMD/.emacs.d/issues/38#issuecomment-706657288)
-   and more&#x2026; Love you guys! ❤️❤️


<a id="org2358066"></a>

## About README

This README is originated from `init.org` that is generated using `M-x org-gfm-export-to-markdown`. Every block of code is generated through this function - it exports sections of code from the `elisp/` directory. You will not see their presence in `init.org`. This not only enables a cleaner organization but also significantly improves Emacs start-up time than the traditional *everything in an org file* approach.


<a id="org210249f"></a>

## Installation

1.  Install [GNU Emacs](https://www.gnu.org/software/emacs/).
    -   (Optional) On Ubuntu, `emacs-snapshot` is a great way to get latest version of Emacs.
        
        ```bash
        sudo add-apt-repository -y ppa:ubuntu-elisp
        sudo apt-get update
        sudo apt-get install emacs-snapshot
        ```
    -   (Optional) Build latest Emacs from source.
        
        ```bash
        # Install essential build tools
        sudo apt-get install build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev gnutls-dev libgtk-3-dev git autoconf
        # Clone source
        git clone --depth=1 https://github.com/emacs-mirror/emacs.git
        # Go to source
        cd emacs/
        # Build Emacs
        ./autogen.sh
        ./configure --with-mailutils
        make
        # Install (optional)
        sudo make install
        ```
2.  Clone this repo to `$HOME`.
    
    ```bash
    git clone https://github.com/MatthewZMD/.emacs.d.git ~/.emacs.d
    ```
3.  Ensure a stable connection to Github, then open Emacs.
4.  In your favorite browser, `Ctrl-f Prerequisite` through this README and follow the **Prerequisite** instructions.
5.  Restart Emacs.


<a id="org0360179"></a>

## Modification

You have the permission to use, modify, distribute in any way you want.

However, what is *free* stays *free*. After all, this is [GPL](LICENSE).

**Remember** you must manually sync this README with all the new changes you made by:

1.  Please do **NOT** edit this `README.md` file, edit `init.org` instead!
2.  If you add a new mode, create a new `<file-name>.el` file in `elisp/` directory.
3.  Put `(require '<file-name>)` in [init.el](init.el) accordingly.
4.  Add `#+INCLUDE: "~/.emacs.d/elisp/<place-holder>.el" src emacs-lisp :range-begin "<start-line-wrapper-exclusive>" :range-end "<end-line-wrapper-exclusive>"` in the appropriate section in `init.org`.
5.  Enter `C-x C-s` to save and update `:lines`. (if you don't see the updated effect, run `M-x save-and-update-includes` manually)
6.  Call `M-x org-gfm-export-to-markdown` to update `README.md` automatically.


<a id="orgf7750f8"></a>

## Contribution

If you spotted a bug or you have any suggestions, please fill in an issue. If you have something to fix, feel free to create a pull request.


<a id="org9ef0b91"></a>

## Special Thanks

Everyone starts somewhere, and I started here.

-   [Vincent Zhang's Centaur Emacs](https://github.com/seagle0128/.emacs.d)
-   [Henrik Lissner's Doom Emacs](https://github.com/hlissner/doom-emacs)
-   [Poncie Reyes's .emacs.d](https://github.com/poncie/.emacs.d)


<a id="orgfd1bec8"></a>

# Startup


<a id="org98c2aae"></a>

## Lexical Binding

Enable lexical binding for better variable scoping. [Why?](https://nullprogram.com/blog/2016/12/22/)

> Until Emacs version 24.1 (June 2012), Elisp predominantly utilized dynamically scoped variables, a characteristic common in older Lisp dialects. While dynamic scope has its specific applications, it is generally deemed unsuitable for local variables, and very few modern programming languages embrace it.

```emacs-lisp
;;; init.el --- -*- lexical-binding: t -*-
```


<a id="org146f8ce"></a>

## Early Init

Emacs 27 introduces `early-init.el`, a configuration file that executes prior to `init.el`, coinciding with package and UI initialization.


<a id="org181556c"></a>

### Compatibility With 26

Ensure the configuration accommodates both versions by checking if the `emacs-version >` 26= and manually requiring `early-init` settings if `emacs-version < 27`.

```emacs-lisp
(cond ((version< emacs-version "26.1")
       (warn "M-EMACS requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (require 'early-init))))
```


<a id="org8abbbba"></a>

### Defer Garbage Collection

Postpone garbage collection earlier in the startup sequence to improve performance, as highlighted by [hlissner](https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly).

> Garbage collection can significantly slow down startup time, often doubling it. The key is to raise the memory threshold as early as possible.

```emacs-lisp
(setq gc-cons-threshold 100000000)
```


<a id="org3890555"></a>

### Disable `package-enable-at-startup`

Package initialization occurs automatically before loading the user configuration, which means we need to prevent Emacs from executing it prematurely.

```emacs-lisp
(setq package-enable-at-startup nil)
```


<a id="orgebf6495"></a>

### Unset `file-name-handler-alist`

During startup, Emacs doesn't require specific file handlers for every file it opens or loads; thus, we should unset this list to optimize the startup process.

```emacs-lisp
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
```


<a id="orgb62feb5"></a>

### Disable `site-run-file`

```emacs-lisp
(setq site-run-file nil)
```


<a id="org357eb8a"></a>

### Disable Unnecessary Interface

Disabling unnecessary interfaces at this stage enhances speed before they are initialized.

```emacs-lisp
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
```


<a id="org125bd13"></a>

## Garbage Collection


<a id="org89d2d11"></a>

### Adjust `gc-cons-threshold` for Interactive Use

A excessively high `gc-cons-threshold` can lead to freezing and stuttering during prolonged interactive sessions. If stuttering occurs, increase the threshold; if freezing happens, decrease it.

```emacs-lisp
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.

If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
```

Additionally, enabling garbage collection when Emacs loses focus and minimizing it during the use of the minibuffer can enhance responsiveness.

```emacs-lisp
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
```


<a id="org5fc0c29"></a>

## Load Path

Since all configuration files reside in the `elisp/` directory, it is essential to include this path in the `load-path` to ensure proper loading.

```emacs-lisp
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))
```


<a id="org05703f6"></a>

## Define Constants

```emacs-lisp
(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst python-p
  (or (executable-find "python3")
      (and (executable-find "python")
           (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
  "Do we have python3?")

(defconst pip-p
  (or (executable-find "pip3")
      (and (executable-find "pip")
           (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
  "Do we have pip3?")

(defconst clangd-p
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst eaf-env-p
  (and (display-graphic-p) python-p pip-p)
  "Do we have EAF environment setup?")
```


<a id="org77f8e36"></a>

## Load Private File

The `init-private.el` file has been designated within the `user-emacs-directory` for personal configurations you wish to keep outside source control.

```emacs-lisp
;; Load init-private.el if it exists
(when (file-exists-p (expand-file-name "init-private.el" user-emacs-directory))
  (load-file (expand-file-name "init-private.el" user-emacs-directory)))
```


<a id="org2044298"></a>

# Package Management

Some packages are disabled using the `:disabled` tag due to infrequent usage. You can similarly disable packages as needed:

```emacs-lisp
(use-package foo
  :disabled)
```


<a id="orgdc3eee0"></a>

## Straight

[Straight](https://github.com/radian-software/straight.el) is preferred over `package.el` for its declarative and reproducible configuration, ensuring reliable package management and easy updates by utilizing Git for version tracking.

```emacs-lisp
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(setq package-check-signature nil)
```


<a id="org3c64b1e"></a>

## Use Package

[Use-package](https://github.com/jwiegley/use-package) simplifies Emacs package configuration, enhancing performance and clarity. When paired with straight.el, it allows for quick and seamless package management.

```emacs-lisp
(straight-use-package 'use-package)

(eval-and-compile
  (setq use-package-verbose t
        use-package-expand-minimally t
        use-package-compute-statistics t
        use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))
```


<a id="org37a148e"></a>

## Diminish

[Diminish](https://github.com/emacsmirror/diminish) can remove certain minor modes from the mode-line to declutter the interface.

```emacs-lisp
(use-package diminish)
```


<a id="orgef1d046"></a>

# Global Functionalities


<a id="orge640cc4"></a>

## User Information

**Prerequisite**:

-   Feel free to update this section with your information

```emacs-lisp
(setq user-full-name "John Doe")
(setq user-mail-address "johndoe@johndoe.net")
```


<a id="orgf057944"></a>

## Bindings

```emacs-lisp
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
;; Revert buffer
(global-set-key (kbd "<f5>") #'revert-buffer-quick)
```


<a id="orgc54599e"></a>

## Avy

[Avy](https://github.com/abo-abo/avy) offers an efficient method for navigating text.

```emacs-lisp
(use-package avy
  :defer t
  :bind
  (("C-z c" . avy-goto-char-timer)
   ("C-z l" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))));
```


<a id="org767e64d"></a>

## Crux

[Crux](https://github.com/bbatsov/crux) is a collection of incredibly useful extensions for Emacs, enhancing functionality and ease of use.

```emacs-lisp
(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   ("C-k" . crux-smart-kill-line))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))
```


<a id="org179d423"></a>

## Ivy, Amx, Counsel, Swiper

[Ivy](https://github.com/abo-abo/swiper) is a versatile completion mechanism for Emacs. It incorporates tools such as [Amx](https://github.com/DarwinAwardWinner/amx), [Counsel](https://github.com/abo-abo/swiper), and [Swiper](https://github.com/abo-abo/swiper) to enhance the user experience.

```emacs-lisp
(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  (("C-s" . swiper-isearch)
   ("C-z s" . counsel-rg)
   ("C-z b" . counsel-buffer-or-recentf)
   ("C-z C-b" . counsel-ibuffer)
   ("M-y" . counsel-yank-pop)
   (:map ivy-minibuffer-map
         ("M-RET" . ivy-immediate-done))
   (:map counsel-find-file-map
         ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
    (ivy--cd "~/")))
```


<a id="orgaab6f97"></a>

## Color Ripgrep

[Color rg](https://github.com/manateelazycat/color-rg) is a search and refactoring tool built on *ripgrep*, designed to search text efficiently. **Prerequisite**: Ensure that [ripgrep](https://github.com/BurntSushi/ripgrep#installation) is installed and the \`rg\` command is included in your \`PATH\`.

```emacs-lisp
(use-package color-rg
  :straight (color-rg :type git :host github :repo "manateelazycat/color-rg")
  :if (executable-find "rg")
  :bind ("C-M-s" . color-rg-search-input))
```


<a id="org0975333"></a>

## Find File In Project

[Find File In Project](https://github.com/technomancy/find-file-in-project) provides quick access to files within a project in Emacs. **Prerequisite**: Ensure \`GNU Find\` is in your \`PATH\`, and install [Gow](https://github.com/bmatzelle/gow), Cygwin, or MSYS2 on Windows to use this feature.

```emacs-lisp
(use-package find-file-in-project
  :if (executable-find "find")
  :init
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t))
  :bind (("C-z o" . ffap)
         ("C-z p" . ffip)))
```


<a id="org8d5e9e9"></a>

## Files Directories


<a id="org7fe3a22"></a>

### Dired

Dired serves as the directory editor in Emacs, facilitating file management.

```emacs-lisp
(use-package dired
  :straight (:type built-in)
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ;; Always delete and copy recursively
  (dired-listing-switches "-lah")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))
```


<a id="org3a7445c"></a>

### Disk Usage

[Disk Usage](https://gitlab.com/ambrevar/emacs-disk-usage) is a file system analyzer that provides a tabulated view of file listings sorted by size, helping you manage disk space.

```emacs-lisp
(use-package disk-usage
  :commands (disk-usage))
```


<a id="orgad53fd9"></a>

### Save All Buffers

```emacs-lisp
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))
(global-set-key (kbd "C-x C-s") nil)
(global-set-key (kbd "C-x C-s") #'save-all-buffers)
```


<a id="org01756db"></a>

## Winner

Winner mode allows you to restore previous window layouts, providing a quick way to manage your workspace.

```emacs-lisp
(use-package winner
  :straight (:type built-in)
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))
```


<a id="org0b213ee"></a>

## Which Key

[Which Key](https://github.com/justbur/emacs-which-key) displays key bindings that follow an incomplete command, enhancing usability by reminding users of available options.

```emacs-lisp
(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))
```


<a id="org869a304"></a>

## Undo Tree

[Undo tree](https://www.emacswiki.org/emacs/UndoTree) visualizes the history of changes made in a file, making it easier to manage and navigate undo operations.

```emacs-lisp
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t))
```


<a id="orgb1bb92b"></a>

## Discover My Major

[Discover my major](https://github.com/jguenther/discover-my-major) helps you explore key bindings and their meanings for the current Emacs major mode, which enhances the learning experience.

```emacs-lisp
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))
```


<a id="org659b5b1"></a>

## Ace Window

[Ace Window](https://github.com/abo-abo/ace-window) enables you to efficiently select and switch between windows in Emacs.

```emacs-lisp
(use-package ace-window
  :bind ("C-x C-o" . ace-window))
```


<a id="org7b861c9"></a>

## Terminal


<a id="org9ef1923"></a>

### Vterm

[Vterm](https://github.com/akermu/emacs-libvterm) is fully-fledged terminal emulator inside GNU Emacs based on libvterm, a C library. As a result of using compiled code (instead of elisp), emacs-libvterm is fully capable, fast, and it can seamlessly handle large outputs.

```emacs-lisp
(use-package vterm
  :commands vterm
  :bind ((:map vterm-mode-map
               ("C-y" . vterm-yank)
               ("M-y" . vterm-yank-pop)
               ("C-q" . vterm-send-next-key)
               ("C-z" . nil)
               ("M-:" . nil)))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s"))
```


<a id="org966940d"></a>

### Shell Here

[Shell Here](https://github.com/ieure/shell-here) opens a shell buffer within the context of the current \`default-directory\`, providing quick terminal access.

```emacs-lisp
(use-package shell-here
  :bind ("M-~" . shell-here)
  :config
  (when *sys/linux*
    (setq explicit-shell-file-name "/bin/bash")))
```


<a id="org3f055f3"></a>

### Multi Term

[Multi Term](https://github.com/manateelazycat/multi-term) is a terminal management mode that allows you to handle multiple terminal buffers conveniently within Emacs.

```emacs-lisp
(use-package multi-term
  :straight (multi-term :type git :host github :repo "manateelazycat/multi-term")
  :commands (multi-term)
  :bind
  (("M-$" . multi-term)
   (:map dired-mode-map ("M-$" . multi-term)))
  :custom
  (multi-term-program (executable-find "bash"))
  (term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("C-v" . scroll-up-command)
     ("M-v" . scroll-down-command)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("<M-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-d" . term-send-delete-word)
     ("M-," . term-send-raw)
     ("M-." . comint-dynamic-complete))))
```


<a id="org21a4e4c"></a>

### Term Keys

[Term Keys](https://github.com/CyberShadow/term-keys) provides seamless keyboard input for Emacs in terminal emulators, ensuring consistent performance.

```emacs-lisp
(use-package term-keys
  :straight (term-keys :type git :host github :repo "CyberShadow/term-keys")
  :if (not (display-graphic-p))
  :config (term-keys-mode t))
```


<a id="orgce1e8d8"></a>

### Exec Path From Shell

[Exec Path From Shell](https://github.com/purcell/exec-path-from-shell) ensures that environment variables in Emacs match those of the user's shell, maintaining consistency across different environments.

```emacs-lisp
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH"
     "OPENAI_API_KEY" "ANTHROPIC_API_KEY"
     "XAI_API_KEY" "DEEPSEEK_API_KEY"
     "OPENROUTER_API_KEY" "GEMINI_API_KEY"))
  :config
  (exec-path-from-shell-initialize))
```


<a id="orge64ac27"></a>

## Sudo Edit

[Sudo Edit](https://github.com/nflath/sudo-edit) allows you to open files with \`sudo\`, enabling easier access to protected files.

```emacs-lisp
(use-package sudo-edit
  :commands (sudo-edit))
```


<a id="org00f5682"></a>

## Ibuffer

[Ibuffer](https://www.emacswiki.org/emacs/IbufferMode) is an advanced alternative to BufferMenu that allows you to manage buffers similarly to how Dired handles files, vastly improving efficiency. It integrates with [IBuffer VC](https://github.com/purcell/ibuffer-vc), which groups buffers by git project and displays file state.

```emacs-lisp
(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer)
  :init
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))
```


<a id="orgcbd3dd0"></a>

## Config

A collection of essential configurations that greatly enhance usability and productivity.


<a id="org747536f"></a>

### UTF-8 Coding System

Configure Emacs to utilize UTF-8 encoding with Unix line endings for optimal compatibility.

```emacs-lisp
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
```


<a id="orge01e63f"></a>

### Optimize Editing Experience

```emacs-lisp
;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

(defun toggle-auto-trailing-ws-removal ()
  "Toggle trailing whitespace removal."
  (interactive)
  (if (member #'smart-delete-trailing-whitespace before-save-hook)
      (progn
        (remove-hook 'before-save-hook #'smart-delete-trailing-whitespace)
        (message "Disabled auto remove trailing whitespace."))
    (add-hook 'before-save-hook #'smart-delete-trailing-whitespace)
    (message "Enabled auto remove trailing whitespace.")))
;; Add to hook during startup
(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

;; Replace selection on insert
(delete-selection-mode 1)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
```


<a id="orgad3167d"></a>

### History

This section manages aspects of the editing history to enhance user experience.

```emacs-lisp
(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
```


<a id="orgb28783b"></a>

### Small Configs

```emacs-lisp
;; Move the backup fies to user-emacs-directory/.backup
(setq backup-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))

;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Enable `erase-buffer' function
(put 'erase-buffer 'disabled nil)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
```


<a id="org4df23aa"></a>

## Functions

A selection of important functions to streamline your workflow.


<a id="org8df0b34"></a>

### Resize Window Width / Height Functions

```emacs-lisp
;; Resizes the window width based on the input
(defun resize-window-dimension (dimension)
  "Resize window by DIMENSION (width or height) with percentage input."
  (lambda (percent)
    (interactive (list (if (> (count-windows) 1)
                          (read-number (format "Set current window %s in [1~9]x10%%: " dimension))
                        (error "You need more than 1 window to execute this function!"))))
    (message "%s" percent)
    (let ((is-width (eq dimension 'width)))
      (window-resize nil
                    (- (truncate (* (/ percent 10.0)
                                   (if is-width (frame-width) (frame-height))))
                       (if is-width (window-total-width) (window-total-height)))
                    is-width))))

(defalias 'resize-window-width (resize-window-dimension 'width)
  "Resizes the window width based on percentage input.")
(defalias 'resize-window-height (resize-window-dimension 'height)
  "Resizes the window height based on percentage input.")

;; Setup shorcuts for window resize width and height
(global-set-key (kbd "C-z w") #'resize-window-width)
(global-set-key (kbd "C-z h") #'resize-window-height)

(defun resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

;; Setup shorcuts for window resize width and height
(defun window-width-increase ()
  (interactive)
  (resize-window t 5))

(defun window-width-decrease ()
  (interactive)
  (resize-window t -5))

(defun window-height-increase ()
  (interactive)
  (resize-window nil 5))

(defun window-height-decrease ()
  (interactive)
  (resize-window nil -5))

(global-set-key (kbd "M-W =") #'window-width-increase)
(global-set-key (kbd "M-W M-+") #'window-width-increase)
(global-set-key (kbd "M-W -") #'window-width-decrease)
(global-set-key (kbd "M-W M-_") #'window-width-decrease)

(global-set-key (kbd "M-Q =") #'window-height-increase)
(global-set-key (kbd "M-Q M-+") #'window-height-increase)
(global-set-key (kbd "M-Q -") #'window-height-decrease)
(global-set-key (kbd "M-Q M-_") #'window-height-decrease)
```


<a id="orgccfd0a9"></a>

### Edit This Configuration File Shortcut

```emacs-lisp
(defun edit-configs ()
  "Opens the README.org file."
  (interactive)
  (find-file "~/.emacs.d/init.org"))

(global-set-key (kbd "C-z e") #'edit-configs)
```


<a id="orgb22e2ae"></a>

### Update Org Mode Include Automatically

Automatically updates Org Mode INCLUDE statements based on guidance from [Artur Malabarba](http://endlessparentheses.com/updating-org-mode-include-statements-on-the-fly.html).

```emacs-lisp
(defun save-and-update-includes ()
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in `org-mode', so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

(add-hook 'before-save-hook #'save-and-update-includes)

(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" (+ l 1) (- r 1)))))) ;; Exclude wrapper
```


<a id="orgcd7ee87"></a>

### MiniBuffer Functions

```emacs-lisp
(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when using the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)

;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
```


<a id="org66708a4"></a>

### Display Line Overlay

```emacs-lisp
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:background null :inherit highlight)))
    ol))
```


<a id="orge44f1a2"></a>

### Read Lines From File

```emacs-lisp
(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer (insert-file-contents file-path)
                    (split-string (buffer-string) "\n" t)))
```


<a id="org2229b8c"></a>

### Where Am I

```emacs-lisp
(defun where-am-i ()
  "An interactive function showing function `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))
```


<a id="orgb86987f"></a>

# UI Enhancements


<a id="org634c3cb"></a>

## Doom Themes

[Doom Themes](https://github.com/hlissner/emacs-doom-themes) is a powerful UI plugin that provides a comprehensive collection of themes to enhance visual aesthetics in Emacs.

```emacs-lisp
(use-package doom-themes
  :custom-face
  (cursor ((t (:background "Red"))))
  :config
  ;; flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-one t)
  (defun switch-theme ()
    "An interactive funtion to switch themes."
    (interactive)
    (when custom-enabled-themes
      (disable-theme (intern (car (mapcar #'symbol-name custom-enabled-themes)))))
    (call-interactively #'load-theme)))
```


<a id="org511f2d0"></a>

## Doom Modeline

[Doom Modeline](https://github.com/seagle0128/doom-modeline) offers a feature-rich modeline, inspired by DOOM Emacs, that is both faster and more powerful than traditional modelines.

```emacs-lisp
(use-package doom-modeline
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  :config
  (doom-modeline-mode))
```


<a id="org9296872"></a>

## Dashboard


<a id="org9645cb7"></a>

### Dashboard

[Dashboard](https://github.com/rakanalh/emacs-dashboard) is an extensible startup screen for Emacs, providing a customizable interface when launching the application. Choose either `KEC_Dark_BK.png` or `KEC_Light_BK.png` depending on your preferred background theme.

```emacs-lisp
(use-package dashboard
  :demand
  :diminish (dashboard-mode page-break-lines-mode)
  :bind
  (("C-z d" . open-dashboard)
   :map dashboard-mode-map
   (("n" . dashboard-next-line)
    ("p" . dashboard-previous-line)
    ("N" . dashboard-next-section)
    ("F" . dashboard-previous-section)))
  :custom
  (dashboard-banner-logo-title "Close the world. Open the nExt.")
  (dashboard-startup-banner (expand-file-name "images/KEC_Dark_BK_Small.png" user-emacs-directory))
  (dashboard-items '((recents  . 7)
                     (bookmarks . 7)
                     (agenda . 5)))
  (initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   (if (featurep 'all-the-icons)
       `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust -0.05)
           "M-EMACS" "Browse M-EMACS Homepage"
           (lambda (&rest _) (browse-url "https://github.com/MatthewZMD/.emacs.d")))
          (,(all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.1)
           "Configuration" "" (lambda (&rest _) (edit-configs)))
          (,(all-the-icons-faicon "cogs" :height 1.0 :v-adjust -0.1)
           "Update" "" (lambda (&rest _) (auto-package-update-now)))))
     `((("" "M-EMACS" "Browse M-EMACS Homepage"
         (lambda (&rest _) (browse-url "https://github.com/MatthewZMD/.emacs.d")))
        ("" "Configuration" "" (lambda (&rest _) (edit-configs)))
        ("" "Update" "" (lambda (&rest _) (auto-package-update-now)))))))
  :custom-face
  (dashboard-banner-logo-title ((t (:family "Love LetterTW" :height 123))))
  :config
  (dashboard-setup-startup-hook)
  ;; Open Dashboard function
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (delete-other-windows)))
```


<a id="org0a106c0"></a>

### Page Break Lines

[Page-break-lines](https://github.com/purcell/page-break-lines) displays form feed characters as clean, horizontal rules, improving readability.

```emacs-lisp
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))
```


<a id="orgd2c485a"></a>

## Fonts and Icons

**Prerequisite**: Install all available fonts and icons from the \`fonts/\` directory. Then execute `M-x all-the-icons-install-fonts` and `M-x nerd-icons-install-fonts` to apply them.


<a id="orga705052"></a>

### Fonts

```emacs-lisp
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar font-list '(("Input" . 11) ("Hack" . 12) ("Consolas" . 12) ("Love LetterTW" . 12.5))
  "List of fonts and sizes.  The first one available will be used.")
```

Function dedicated to switching between installed fonts seamlessly.

```emacs-lisp
(defun get-available-fonts ()
  "Get list of available fonts from font-list."
  (let (available-fonts)
    (dolist (font font-list (nreverse available-fonts))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))))

(defun change-font ()
  "Interactively change a font from a list a available fonts."
  (interactive)
  (let* ((available-fonts (get-available-fonts))
         font-name font-size font-setting)
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(when (display-graphic-p)
  (change-font))
```


<a id="orge3f0e94"></a>

### All The Icons

[All The Icons](https://github.com/domtronn/all-the-icons.el) is a utility package designed to aggregate various icon fonts, specifically for GUI Emacs.

```emacs-lisp
(use-package all-the-icons :if (display-graphic-p))
```


<a id="org0cfdddd"></a>

## Smooth Scrolling

Configuration settings are provided to enable smooth scrolling in Emacs, enhancing reading and navigation comfort.

```emacs-lisp
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
```


<a id="org18d86e8"></a>

## Highlight Lines

```emacs-lisp
(global-hl-line-mode 1)
```


<a id="org20e81a9"></a>

## Prettify Symbols

[Prettify symbols mode](https://www.emacswiki.org/emacs/PrettySymbol) is a built-in feature that enables the display of character sequences as aesthetically pleasing symbols, improving code readability.

```emacs-lisp
(defun setup-prettify-symbols ()
  "Setup prettify-symbols-mode with predefined symbols."
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805)))
  (prettify-symbols-mode 1))

(global-prettify-symbols-mode 1)
(add-hook 'prog-mode-hook #'setup-prettify-symbols)
(add-hook 'org-mode-hook #'setup-prettify-symbols)
```


<a id="org58c907e"></a>

## UI Configs


<a id="orged05310"></a>

### Title Bar

```emacs-lisp
(setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
```


<a id="org26835ec"></a>

### Simplify Yes/No Prompts

```emacs-lisp
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)
```


<a id="org2b9062c"></a>

### Disable Splash Screen

```emacs-lisp
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
;; https://www.youtube.com/watch?v=NfjsLmya1PI
(setq initial-scratch-message "Present Day, Present Time...\n")
```


<a id="orgaf76dd1"></a>

### Line Numbers

Configure Emacs to display both line and column numbers in the modeline for better code navigation.

```emacs-lisp
;; Hook line numbers to only when files are opened, also use linum-mode for emacs-version< 26
(if (version< emacs-version "26")
    (global-linum-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))
;; Display column numbers in modeline
(column-number-mode 1)
```


<a id="orgdab350b"></a>

### Modeline Time and Battery

This feature displays time and battery statistics in the modeline, providing useful information at a glance.

```emacs-lisp
(display-time-mode 1)
(when (and battery-status-function
           (not (string-match-p "N/A" (battery-format "%B" (funcall battery-status-function)))))
  (display-battery-mode 1))
```


<a id="orgb54bf7a"></a>

### Pixel Scroll Precision Mode

Pixel scroll precision mode, introduced in Emacs 29.1, enables finer scrolling control within a buffer, displaying content pixel-by-pixel for increased precision.

```emacs-lisp
(when (version<= "29.1" emacs-version)
  (pixel-scroll-precision-mode 1))
```


<a id="org5eb09a6"></a>

# General Programming


<a id="org3670e66"></a>

## Aidermacs

[Aidermacs](https://github.com/MatthewZMD/aidermacs), Aider AI Pair Programming for Emacs

```emacs-lisp
(use-package aidermacs
  :if (executable-find "aider")
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-backend 'comint)
  (aidermacs-auto-commits nil)
  (aidermacs-default-model "openrouter/anthropic/claude-3.7-sonnet")
  :config
  (add-to-list 'display-buffer-alist
               `("\\*aidermacs.*\\*"
                 (display-buffer-pop-up-window)))
  :bind
  (("C-z a" . aidermacs-transient-menu)))
;; -AidermacsPac
```


<a id="org8334fce"></a>

## Magit

[Magit](https://magit.vc/) provides a user-friendly interface for interacting with the Git version control system, streamlining version management tasks.

```emacs-lisp
(use-package magit
  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :config
  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t)))
```


<a id="org011135e"></a>

## Projectile

[Projectile](https://github.com/bbatsov/projectile) is a powerful project interaction library that simplifies navigating and managing projects in Emacs. **Prerequisite**: For Windows OS users, install [Gow](https://github.com/bmatzelle/gow/releases) and ensure it is added to the \`PATH\`. [Gow](https://github.com/bmatzelle/gow) is a handy lightweight installer that facilitates the use of various open source UNIX applications compiled as native Win32 binaries. The \`tr\` command is particularly needed for Projectile's alien indexing.

```emacs-lisp
(use-package projectile
  :bind
  ("C-x p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (when (and *sys/win32*
             (executable-find "tr"))
    (setq projectile-indexing-method 'alien))
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))
```


<a id="org0241b96"></a>

## YASnippet


<a id="orgf0863f6"></a>

### YASnippet

[YASnippet](https://github.com/joaotavora/yasnippet) is a versatile programming template system for Emacs. It can load [YASnippet Snippets](https://github.com/AndreaCrotti/yasnippet-snippets), which is a rich collection of snippets for a variety of languages.

```emacs-lisp
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :after yasnippet)
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (yas-reload-all)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))
```


<a id="orgecaed7a"></a>

## Treesit Parser Manager

[treesit-auto](https://github.com/renzmann/treesit-auto) simplifies installation/management of tree-sitter grammars. Automatically handles grammar compilation/updates for multiple languages.

**Prerequisite**: Run `M-x treesit-auto-install-all` to install grammars.

> This package is, admittedly, a hack. treesit.el provides an excellent foundation for incremental source code parsing in Emacs 29. Over time this foundation will expand into an improved core editing experience. While this package will likely become obsolete in Emacs 30+ (which may have built-in alternatives), it still provides quality-of-life improvements for Emacs 29 users.

```emacs-lisp
(use-package treesit-auto
  :if (version<= "29" emacs-version)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (when (version<= "30" emacs-version)
    (error "The treesit-auto package maybe obsolete!"))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (defun treesit-show-parser-used-at-point ()
    "Shows treesit parser used at point."
    (interactive)
    (if (and (fboundp 'treesit-available-p)
             (treesit-available-p))
        (message (format "%s" (treesit-language-at (point))))
      (message "treesit is not available"))))
```


<a id="org8ef21a8"></a>

## Dumb Jump

[Dumb jump](https://github.com/jacktasia/dumb-jump) allows for swift navigation to definition within your codebase, enhancing the coding experience.

```emacs-lisp
(use-package dumb-jump
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ("C-c C-j" . dumb-jump-go)
         ("C-c C-i" . dumb-jump-go-prompt)))
  :custom (dumb-jump-selector 'ivy))
```


<a id="org2b8330f"></a>

## Parenthesis


<a id="orgaf22c8a"></a>

### Smartparens

[Smartparens](https://github.com/Fuco1/smartparens) is a minor mode designed for effectively handling paired constructs, streamlining coding involving parentheses and brackets.

```emacs-lisp
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-a" . sp-backward-down-sexp)
        ("C-M-e" . sp-up-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-k" . sp-change-enclosing)
        ("M-k" . sp-kill-sexp)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ("C-]" . sp-select-next-thing-exchange))
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil))
```


<a id="org70a5f7b"></a>

### Match Parenthesis

This feature ensures that parentheses are matched and automatically paired while providing visual cues even when they are offscreen, enhancing code clarity.

```emacs-lisp
;; Show matching parenthesis
(show-paren-mode 1)
;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)

;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)
(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
    (defun show-paren--off-screen+ (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp ov)
        (delete-overlay ov))
      ;; check if it's appropriate to show match info,
      ;; see `blink-paren-post-self-insert-function'
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; rebind `minibuffer-message' called by
        ;; `blink-matching-open' to handle the overlay display
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq ov (display-line-overlay+
                                 (window-start) msg))))))
          (blink-matching-open))))))
```


<a id="orge9ba2a4"></a>

## Indentation

[Indent Bars](https://github.com/jdtsmith/indent-bars) is a customizable indentation guide that provides fast and efficient visual cues for code structure in Emacs.

```emacs-lisp
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters
                      list list_comprehension
                      dictionary dictionary_comprehension
                      parenthesized_expression subscript)))
  (indent-bars-pattern ". . . . ")
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.2)
  (indent-bars-zigzag 0.1)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth '(:pattern "." :pad 0.1 :width 0.45))
  :hook ((prog-mode yaml-mode) . indent-bars-mode))
```

This section also covers indentation configuration for optimal coding experiences.

```emacs-lisp
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)
(c-set-offset 'comment-intro 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label 0)
(c-set-offset (quote cpp-macro) 0 nil)
(defun smart-electric-indent-mode ()
  "Disable 'electric-indent-mode in certain buffers and enable otherwise."
  (cond ((and (eq electric-indent-mode t)
              (member major-mode '(erc-mode text-mode)))
         (electric-indent-mode 0))
        ((eq electric-indent-mode nil) (electric-indent-mode 1))))
(add-hook 'post-command-hook #'smart-electric-indent-mode)
```


<a id="org32752b2"></a>

## Format All

[Format all](https://github.com/lassik/emacs-format-all-the-code) provides a convenient feature to auto-format source code, catering to numerous programming languages. **Prerequisite**: Consult [Supported Languages](https://github.com/lassik/emacs-format-all-the-code#supported-languages) to identify which additional tools are necessary for specific languages.

```emacs-lisp
(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))
```


<a id="orgd6afb6c"></a>

## Ediff

[Ediff](https://www.gnu.org/software/emacs/manual/html_mono/ediff.html) enables users to compare differences between pairs of files or buffers simultaneously, streamlining the process of resolving discrepancies.

```emacs-lisp
(use-package ediff
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))
```


<a id="org84cc902"></a>

## Package Lint

[Package Lint](https://github.com/purcell/package-lint) checks the current buffer for package.el formatting issues, ensuring your Emacs Lisp packages follow best practices and meet the requirements for submission to MELPA or GNU ELPA.

```emacs-lisp
(use-package package-lint
  :commands (package-lint-current-buffer package-lint-buffer))
```


<a id="orgba2397c"></a>

## Evil Nerd Commenter

[Evil Nerd Commenter](https://github.com/redguardtoo/evil-nerd-commenter) assists users in efficiently commenting out sections of code, enhancing productivity when writing or debugging.

```emacs-lisp
(use-package evil-nerd-commenter
  :bind
  (("C-c M-;" . c-toggle-comment-style)
   ("M-;" . evilnc-comment-or-uncomment-lines)))
```


<a id="org7344cdd"></a>

## Editing


<a id="orgd479f71"></a>

### Iedit

[Iedit](https://github.com/victorhge/iedit) is a versatile minor mode that facilitates simultaneous editing of multiple regions within a buffer or a selected region, streamlining the editing process.

```emacs-lisp
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)
```


<a id="orgea2f029"></a>

### Delete Block

[Delete Block](https://github.com/manateelazycat/delete-block) provides an efficient method for deleting blocks of text or code, promoting a smoother editing workflow.

```emacs-lisp
(use-package delete-block
  :straight (delete-block :type git :host github :repo "manateelazycat/delete-block")
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))
```


<a id="org30d29e7"></a>

## Headers

[Header2](https://www.emacswiki.org/emacs/header2.el) simplifies the process of creating and updating file headers, automating documentation tasks.

```emacs-lisp
(use-package header2
  :straight (header2 :type git :host github :repo "emacsmirror/header2")
  :custom
  (header-copyright-notice (concat "Copyright (C) 2019 " (user-full-name) "\n"))
  :hook (emacs-lisp-mode . auto-make-header)
  :config
  (add-to-list 'write-file-functions 'auto-update-file-header)
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2"))
```


<a id="org669b6f2"></a>

## Jupyter Notebook

[Emacs IPython Notebook](https://github.com/millejoh/emacs-ipython-notebook) serves as a client for [Jupyter](https://jupyter.org/), previously known as IPython, allowing for interactive coding sessions within Emacs.


<a id="org0e446fe"></a>

### Usage

1.  Execute `M-x ein:run` to initiate a local Jupyter session.
2.  Login with `M-x ein:login` to connect to a local or remote session.
3.  Open a `.ipynb` file and press `C-c C-o`.

```emacs-lisp
(use-package ein
  :if (executable-find "jupyter")
  :bind
  (("C-c e" . ein:worksheet-execute-cell)
   ("C-c C-e" . ein:worksheet-execute-all-cells))
  :custom-face
  (ein:basecell-input-area-face ((t (:extend t :background "#303640"))))
  :defer t
  :custom
  (ein:worksheet-enable-undo t))
```


<a id="org3696d23"></a>

## Completion / LSP

Instead of the widely-used [Company](http://company-mode.github.io/), I have chosen to use [lsp-bridge](https://github.com/manateelazycat/lsp-bridge), which is entirely multi-threaded and adept at handling all completion needs within Emacs.

```emacs-lisp
(use-package lsp-bridge
  :straight (lsp-bridge
             :type git
             :host github
             :repo "manateelazycat/lsp-bridge"
             :files ("*"))
  :defer 1
  :commands (global-lsp-bridge-mode lsp-bridge-mode)
  :custom
  (acm-enable-codeium nil)
  (acm-enable-tabnine nil)
  (acm-enable-yas nil)
  (acm-enable-quick-access t)
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-python-lsp-server "pyright")
  :bind (("M-." . lsp-bridge-find-def)
         ("M-," . lsp-bridge-find-def-return)
         ("M-i" . lsp-bridge-popup-documentation)
         ("C-M-." . lsp-bridge-peek)
         :map lsp-bridge-ref-mode-map
         ("n" . lsp-bridge-ref-jump-next-keyword)
         ("p" . lsp-bridge-ref-jump-prev-keyword)
         ("M-n" . lsp-bridge-ref-jump-next-file)
         ("M-p" . lsp-bridge-ref-jump-prev-file)
         ("C-x C-q" . lsp-bridge-ref-switch-to-edit-mode)
         :map lsp-bridge-ref-mode-edit-map
         ("C-x C-q" . lsp-bridge-ref-apply-changed)
         ("C-x C-s" . lsp-bridge-ref-apply-changed)
         ("C-c C-k" . lsp-bridge-ref-quit)
         ("M-n" . lsp-bridge-ref-jump-next-file)
         ("M-p" . lsp-bridge-ref-jump-prev-file)
         :map acm-mode-map
         ([remap next-line] . nil)
         ([remap previous-line] . nil))
  :config
  (global-lsp-bridge-mode))
```


<a id="org9700c02"></a>

# Programming


<a id="org7ea7e6f"></a>

## C/C++/Objective C

**Prerequisite**: Since all completion features are supported by [LSP Mode](https://github.com/emacs-lsp/lsp-mode), it needs to be set up correctly.

-   Install [CMake](https://cmake.org/download/) version 3.8 or higher for all operating systems.
-   For Unix-like OS:
    -   It is recommended to use [CCLS](https://github.com/MaskRay/ccls) as the LSP server. Refer to [build instructions](https://github.com/MaskRay/ccls/wiki/Build) for detailed setup.
    -   Set \`ccls-executable\` to the directory where your CCLS is built.
-   For Windows OS:
    -   Install [MinGW](http://www.mingw.org/wiki/Install_MinGW) for compilation.
    -   Due to the complexities of building CCLS on Windows, it is advisable to install [Clangd](https://clang.llvm.org/extra/clangd/Installation.html) instead and ensure it is available in your \`PATH\`.


<a id="org8e4fa95"></a>

### CCLS

[Emacs CCLS](https://github.com/MaskRay/emacs-ccls) is a client for [CCLS](https://github.com/MaskRay/ccls), which is a language server for C/C++/Objective-C. It supports massive codebases, leveraging the capabilities of libclang for enhanced performance.

```emacs-lisp
(use-package modern-cpp-font-lock
  :diminish t
  :config (modern-c++-font-lock-global-mode t))
;; -CPPFontLockPac

;; GoPac
(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports"))
;; -GoPac

;; RustPac
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run)))
;; -RustPac

(provide 'init-cc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```


<a id="org9dfb98d"></a>

### Modern C++ Font Lock

[Modern CPP Font Lock](https://github.com/ludwigpacifici/modern-cpp-font-lock) enhances syntax highlighting specifically for modern C++ syntax, improving readability and code comprehension.

```emacs-lisp

```


<a id="org2470c2f"></a>

## Golang

[Go Mode](https://github.com/dominikh/go-mode.el) is an Emacs mode specifically designed for Golang programming, providing syntax highlighting and other essential tools. **Prerequisite**: Setting up [gopls](https://github.com/golang/tools/blob/master/gopls/README.md) is necessary for Golang's LSP support.

```bash
go get golang.org/x/tools/gopls@latest
```

```emacs-lisp

```


<a id="org22b3735"></a>

## Rust

[Rust Mode](https://github.com/rust-lang/rust-mode) is tailored for Rust programming within Emacs, ensuring robust development support.

```emacs-lisp

```


<a id="org3b3826f"></a>

## Python

```emacs-lisp
(use-package python-mode
  :straight (:type built-in)
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))
```


<a id="org7bb6fcd"></a>

## ESS

[Emacs Speaks Statistics](https://ess.r-project.org/) (ESS) is designed to facilitate editing scripts and interaction with various statistical analysis programs such as R, S-Plus, SAS, Stata, and OpenBUGS/JAGS. **Prerequisite**: Ensure [R](https://cran.r-project.org/mirrors.html) is installed to utilize ESS effectively with R.

```emacs-lisp
(use-package ess
  :defer t
  :commands R
  :config
  (load "ess-autoloads"))
```


<a id="org72dcef4"></a>

## TeX

**Prerequisite**: Please ensure you have [TeX Live](https://www.tug.org/texlive/quickinstall.html) installed on your system.


<a id="org8170b8f"></a>

### AUCTeX

[AUCTeX](https://www.gnu.org/software/auctex/) is a comprehensive package designed for authoring and formatting TeX documents, supporting multiple TeX macro packages such as AMS-TEX, LaTeX, Texinfo, ConTEXt, and docTEX (dtx files).

```emacs-lisp
(use-package auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (setq reftex-plug-into-AUCTeX t)
                  (reftex-isearch-minor-mode)
                  (setq TeX-PDF-mode t)
                  (setq TeX-source-correlate-method 'synctex)
                  (setq TeX-source-correlate-start-server t)))
  :config
  (when (version< emacs-version "26")
    (add-hook LaTeX-mode-hook #'display-line-numbers-mode)))
;; -AUCTeXPac
```


<a id="orgfcf1481"></a>

## Yaml

[Yaml mode](https://github.com/yoshiki/yaml-mode) is the dedicated major mode for editing files in the YAML data serialization format within Emacs.

```emacs-lisp
(use-package yaml-mode
  :defer t
  :commands (yaml-get-path-at-point)
  :mode "\\.yml\\'"
  :config
  ;; Based on https://github.com/chopmo/dotfiles/blob/master/.emacs.d/customizations/yaml.el
  (defun yaml-indentation-level (s)
    (if (string-match "^ " s)
        (+ 1 (yaml-indentation-level (substring s 1)))
      0))
  (defun yaml-clean-string (s)
    (let* ((s (replace-regexp-in-string "^[ -:]*" "" s))
           (s (replace-regexp-in-string ":$" "" s)))
      s))
  (defun yaml-path-at-point ()
    (save-excursion
      (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
             (level (yaml-indentation-level line))
             result)
        (while (> (point) (point-min))
          (beginning-of-line 0)
          (setq line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (let ((new-level (yaml-indentation-level line)))
            (when (and (string-match "[^[:blank:]]" line)
                       (< new-level level))
              (setq level new-level)
              (setq result (push (yaml-clean-string line) result)))))
        (mapconcat 'identity result " => "))))
  (defun yaml-get-path-at-point ()
    "Display the yaml path at point for 5 seconds"
    (interactive)
    (let ((ov (display-line-overlay+ (window-start) (yaml-path-at-point))))
      (run-with-timer 1 nil (lambda () (when (overlayp ov)
                                         (delete-overlay ov)))))))
```


<a id="org53ebf28"></a>

### Yaml-Pro

[Yaml-pro](https://github.com/zkry/yaml-pro) contains tools for editing YAML leveraging tree-sitter/parser.

```emacs-lisp
(use-package yaml-pro
  :hook (yaml-mode . yaml-pro-mode)
  :bind (("C-c M-p" . yaml-pro-move-subtree-up)
         ("C-c M-n" . yaml-pro-move-subtree-down)))
```


<a id="org81d7913"></a>

## Buildsystem


<a id="org4dc68a0"></a>

### Docker

[Docker](https://github.com/Silex/docker.el) is a mode enabling management of Docker containers directly from Emacs, facilitating container-based workflows.

```emacs-lisp
(use-package docker :defer t)
```

[Dockerfile Mode](https://github.com/spotify/dockerfile-mode) offers specific features for editing Dockerfiles in Emacs.

```emacs-lisp
(use-package dockerfile-mode :defer t)
```


<a id="org9ec277f"></a>

### Groovy

[Groovy Mode](https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes) encompasses a comprehensive major mode for Groovy, grails minor mode, and a groovy inferior mode, catering to Groovy developers.

```emacs-lisp
(use-package groovy-mode :defer t)
```


<a id="org2c434f6"></a>

### Cmake

[Cmake Mode](https://melpa.org/#/cmake-mode) is a library that provides syntax highlighting and indentation functionalities for CMakeLists.txt and \*.cmake files.

```emacs-lisp
(use-package cmake-mode :defer t)
```


<a id="org0e02435"></a>

### Bazel

[Bazel Mode](https://github.com/bazelbuild/emacs-bazel-mode) grants major modes for editing Bazel-specific files including `BUILD` files, `WORKSPACE` files, and `.bazelrc` files, as well as Starlark files.

```emacs-lisp
(use-package bazel :defer t)
```


<a id="orgdefd555"></a>

# Web Development

**Prerequisite**: Install [NodeJS](https://nodejs.org/en/download/) and ensure it is included in your \`PATH\`. Execute the following commands to enable LSP for JavaScript, TypeScript, and HTML:

```bash
npm i -g typescript
npm i -g typescript-language-server
```


<a id="org780ca11"></a>

## Web

[Web mode](https://github.com/fxbois/web-mode) is a specialized major mode designed for editing web templates and related technologies.

```emacs-lisp
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))
```


<a id="org24120a2"></a>

## JavaScript/TypeScript


<a id="org2715a4b"></a>

### JavaScript2

[JS2 mode](https://github.com/mooz/js2-mode) provides an enhanced JavaScript editing experience with features aimed at improving productivity.

```emacs-lisp
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :bind (:map js-mode-map ("M-." . nil)))
```


<a id="org2bba325"></a>

### TypeScript

[TypeScript mode](https://github.com/emacs-typescript/typescript.el) adds dedicated support for TypeScript programming within Emacs, enhancing the development experience.

```emacs-lisp
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))
```


<a id="orga69979e"></a>

### Vue

[Vue mode](https://github.com/AdamNiederer/vue-mode) provides specialized major mode for developing applications using Vue.js, improving the coding workflow.

```emacs-lisp
(use-package vue-mode
  :mode "\\.vue\\'"
  :commands (vue-mode))
```


<a id="org477847a"></a>

## Emmet

[Emmet](https://github.com/smihica/emmet-mode) enables users to write HTML swiftly using CSS-style selectors, enhancing coding efficiency. Refer to [usage instructions](https://github.com/smihica/emmet-mode#usage) for further information.

```emacs-lisp
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))
```


<a id="org20cc77f"></a>

## Instant Rename Tag

[Instant Rename Tag](https://github.com/manateelazycat/instant-rename-tag) offers the functionality to quickly rename HTML tag pairs, serendipitously speeding up markup editing.

```emacs-lisp
(use-package instant-rename-tag
  :straight (instant-rename-tag :type git :host github :repo "manateelazycat/instant-rename-tag")
  :bind ("C-z <" . instant-rename-tag))
```


<a id="org81ce792"></a>

## JSON

[JSON Mode](https://github.com/joshwnj/json-mode) is specifically crafted for editing JSON files, enhancing the formatting and navigation experience.

```emacs-lisp
(use-package json-mode
  :mode "\\.json\\'")
```


<a id="org868fbad"></a>

# Office


<a id="org07377a4"></a>

## Org

[Org](https://orgmode.org/) is a powerful built-in tool in Emacs for note-taking, maintaining TODO lists, project planning, and authoring documents in a fast and efficient plain-text format. **Prerequisite**: Configure `(org-agenda-files (list "~/org/agenda/"))` to specify your agenda folder for using org-agenda. Once this is configured, agenda items tagged with `DEADLINE` or `SCHEDULED` will show up on the [Dashboard](#org9645cb7), which will be updated to provide detailed insights in the [future](https://github.com/MatthewZMD/.emacs.d/issues/37).

```emacs-lisp
(use-package org
  :straight (:type built-in)
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         (:map org-mode-map (("C-c C-p" . eaf-org-export-to-pdf-and-open)
                             ("C-c ;" . nil))))
  :custom
  (org-log-done 'time)
  (calendar-latitude 43.65107) ;; Prerequisite: set it to your location, currently default: Toronto, Canada
  (calendar-longitude -79.347015) ;; Usable for M-x `sunrise-sunset' or in `org-agenda'
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-latex-listings-options '(("breaklines" "true")))
  (org-latex-listings t)
  (org-deadline-warning-days 7)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE" "CANCELED")))
  (org-agenda-window-setup 'other-window)
  (org-latex-pdf-process
   '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (unless (version< org-version "9.2")
    (require 'org-tempo))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (C . t)
     (python . t)
     (plantuml . t)))
  (defun org-export-toggle-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq-local org-latex-listings 'minted)
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

  (defun org-table-insert-vertical-hline ()
    "Insert a #+attr_latex to the current buffer, default the align to |c|c|c|, adjust if necessary."
    (interactive)
    (insert "#+attr_latex: :align |c|c|c|")))
```


<a id="orgbdc3c27"></a>

### Org Roam

[Org Roam](https://www.orgroam.com/) is a personal knowledge management system based on plain text, enabling collection and organization of ideas seamlessly.

```emacs-lisp
(use-package org-roam
  :after org
  :custom
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n h" . org-id-get-create))
  :config
  (when (file-directory-p "~/Documents/roam")
    (setq org-roam-directory (file-truename "~/Documents/roam")))
  (org-roam-db-autosync-mode))
```


<a id="org337cfee"></a>

### HTMLize

[HTMLize](https://github.com/hniksic/emacs-htmlize) is a powerful tool that converts buffer text and its decorations into HTML format, facilitating web integration.

```emacs-lisp
;; -MarkdownModePac
```


<a id="org08c49a3"></a>

### GFM Exporter

[OX-GFM](https://github.com/larstvei/ox-gfm) enables Org Mode to export documents into GitHub Flavored Markdown format, enhancing sharing capabilities.

```emacs-lisp
;; -OXGFMPac
```


<a id="org44dbc9b"></a>

### PlantUML and Graphviz

[PlantUML Mode](https://github.com/skuro/plantuml-mode) offers a dedicated environment for editing PlantUML sources. **Prerequisite**:

1.  Install [plantuml](http://plantuml.com/download) and configure `(org-plantuml-jar-path (expand-file-name "path/to/plantuml.jar"))` to specify its location.
2.  Additionally, install [Graphviz](https://graphviz.gitlab.io/download/) on your system to enable graph visualization. For example, use `sudo apt install graphviz` on Ubuntu to install it.

```emacs-lisp
  :defer t
  :custom
  (org-plantuml-jar-path (expand-file-name "~/tools/plantuml/plantuml.jar")))
;; -PlantUMLPac
```


<a id="org36de13a"></a>

# Multimedia


<a id="org448ca4d"></a>

## EAF

[Emacs Application Framework](https://github.com/manateelazycat/emacs-application-framework) revolutionizes graphical capabilities in Emacs by providing a comprehensive GUI application framework. **Prerequisite**: Ensure that `python3` and `pip3` are installed, then follow the [installation instructions](https://github.com/manateelazycat/emacs-application-framework#install) to get started.

```emacs-lisp
(use-package eaf
  :straight (emacs-application-framework
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*"))
  :if (and eaf-env-p
           (file-directory-p
            (expand-file-name
             "straight/build/emacs-application-framework/app/browser"
             user-emacs-directory)))
  :custom
  (eaf-start-python-process-when-require nil)
  (browse-url-browser-function #'eaf-open-browser) ;; Make EAF Browser my default browser
  (eaf-start-python-process-when-require t)
  (eaf-browser-dark-mode nil)
  (eaf-browser-enable-adblocker t)
  (eaf-webengine-continue-where-left-off t)
  (eaf-webengine-default-zoom 1.25)
  (eaf-webengine-scroll-step 200)
  (eaf-pdf-dark-mode "ignore")
  :demand
  :bind
  (("M-z r" . eaf-open-rss-reader)
   ("M-m r" . eaf-open-rss-reader)
   ("M-#" . eaf-open-pyqterminal))
  :config
  ;; Require all EAF apps unconditionally, change to apps you're interested in.
  (when (require 'eaf-browser nil t)
    (defalias 'browse-web #'eaf-open-browser)
    (eaf-bind-key nil "M-q" eaf-browser-keybinding)
    (eaf-bind-key nil "M-z" eaf-browser-keybinding)
    (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
    (eaf-bind-key open_devtools "M-i" eaf-browser-keybinding)
    (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
    (eaf-bind-key delete_cookies "C-M-q" eaf-browser-keybinding)
    (eaf-bind-key delete_all_cookies "C-M-Q" eaf-browser-keybinding)
    (eaf-bind-key clear_history "C-M-p" eaf-browser-keybinding))
  (when (require 'eaf-pdf-viewer nil t)
    (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
    (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
    (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
    (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding))
  (require 'eaf-file-manager nil t)
  (require 'eaf-music-player nil t)
  (require 'eaf-image-viewer nil t)
  (when (require 'eaf-camera nil t)
    (eaf-bind-key take_photo "p" eaf-camera-keybinding))
  (require 'eaf-demo nil t)
  (require 'eaf-airshare nil t)
  (require 'eaf-markdown-previewer nil t)
  (require 'eaf-video-player nil t)
  (require 'eaf-vue-demo nil t)
  (require 'eaf-file-sender nil t)
  (require 'eaf-mindmap nil t)
  (require 'eaf-netease-cloud-music nil t)
  (require 'eaf-jupyter nil t)
```


<a id="orgd10f5d0"></a>

# Internet


<a id="org4a9e6bf"></a>

## ERC

[Emacs Relay Chat](https://www.emacswiki.org/emacs/ERC) is a modular, extensible IRC client for Emacs, supporting various functionalities like nickname highlighting through [erc-hl-nicks](https://github.com/leathekd/erc-hl-nicks) and image display via [erc-image](https://github.com/kidd/erc-image.el). **Prerequisite**: Add your IRC credentials to the file `~/.authinfo` and configure `my-irc-nick` to specify your IRC nickname.

```text
machine irc.freenode.net login <nickname> password <password> port 6697
```

```emacs-lisp
(use-package erc
  :straight (:type built-in)
  :init
  ;; Prerequisite: Configure this to your IRC nickname
  (defcustom my-irc-nick ""
    "The nickname used to login into ERC"
    :type 'string)
  (use-package erc-hl-nicks :defer t)
  (use-package erc-image :defer t)
  :custom-face
  (erc-notice-face ((t (:foreground "#ababab"))))
  :custom
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs")))
  (erc-user-full-name user-full-name)
  (erc-track-exclude-types '("NICK" "PART" "MODE" "324" "329" "332" "333" "353" "477"))
  (erc-server-coding-system '(utf-8 . utf-8))
  (erc-interpret-mirc-color t)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil)
  (erc-fill-column 100)
  (erc-save-buffer-on-part t)
  (erc-nick-uniquifier "_")
  (erc-log-channels-directory (expand-file-name ".erc-logs" user-emacs-directory))
  :bind
  (("M-z i" . erc-start-or-switch)
   ("M-m i" . erc-start-or-switch)
   ("C-c C-b" . erc-switch-to-buffer)
   (:map erc-mode-map
         ("M-RET" . newline)))
  :hook
  (ercn-notify . erc-notify)
  :config
  (make-directory (expand-file-name ".erc-logs" user-emacs-directory) t)
  (add-to-list 'erc-modules 'notifications)
  (erc-track-mode t)
  (erc-services-mode 1)
  (defun erc-start-or-switch ()
    "Start ERC or switch to ERC buffer if it has started already."
    (interactive)
    (if (get-buffer "irc.libera.chat:6697")
        (erc-track-switch-buffer 1)
      (erc-tls :server "irc.libera.chat" :port 6697 :nick my-irc-nick :full-name user-full-name)))

  (defun erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title))))
```


<a id="orgb134c85"></a>

## MU4E

[Mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html) is a robust email client within Emacs powered by [mu](https://www.djcbsoftware.nl/code/mu/) as its backend. It features [Mu4e Thread Folding](https://github.com/rougier/mu4e-thread-folding) for managing lengthy email threads efficiently. **Note**: This mu4e configuration is tailored specifically for Gmail users. **Prerequisite**:

1.  Set up IMAP using [isync/mbsync](https://wiki.archlinux.org/index.php/Isync) and place your `.mbsyncrc` config in `~/.emacs.d/mu4e/`. A [sample](https://gist.github.com/MatthewZMD/39cc00260486d17450f7228a4f36891f) configuration is available.
2.  Install [mu](https://www.djcbsoftware.nl/code/mu/) for email handling.
3.  Execute the following commands to initialize your email environment.
    
    ```bash
    mkdir -p ~/Maildir/gmail/
    mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -Dmn gmail
    mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a
    mu init --maildir=~/Maildir/ --my-address=YOUR_EMAIL1 --my-address=YOUR_EMAIL2
    mu index
    ```
    
    -   If you encounter an `Invalid Credentials` error while confident of your password correctness, consult [this guide](https://appuals.com/fix-your-imap-server-wants-to-alert-you-invalid-credentials/) for troubleshooting.
4.  (Optional) To track meetings using `org-mode`, assign `gnus-icalendar-org-capture-file` to your designated meeting file.

```emacs-lisp
(use-package mu4e
  :if (executable-find "mu")
  :straight (:type built-in)
  :commands (mu4e make-mu4e-context)
  :init
  (use-package mu4e-alert
    :defer t
    :config
    (when (executable-find "notify-send")
      (mu4e-alert-set-default-style 'libnotify))
    :hook
    ((after-init . mu4e-alert-enable-notifications)
     (after-init . mu4e-alert-enable-mode-line-display)))
  (use-package mu4e-overview :defer t)
  :bind
  (("M-z m" . mu4e)
   ("M-m m" . mu4e)
   (:map mu4e-view-mode-map
         ("e" . mu4e-view-save-attachment)))
  :custom
  (mu4e-maildir (expand-file-name "~/Maildir"))
  (mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a")
  (mu4e-view-prefer-html t)
  (mu4e-update-interval 180)
  (mu4e-headers-auto-update t)
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-images t)
  (mu4e-change-filenames-when-moving t) ; work better for mbsync
  (mu4e-attachment-dir "~/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-results-limit 1000)
  (mu4e-view-use-gnus t)
  (gnus-icalendar-org-capture-file "~/org/agenda/meetings.org") ; Prerequisite: set it to meetings org fie
  (gnus-icalendar-org-capture-headline '("Meetings")) ; Make sure to create Calendar heading first
  :hook
  ((mu4e-view-mode . visual-line-mode)
   (mu4e-compose-mode . (lambda ()
                          (visual-line-mode)
                          (use-hard-newlines -1)
                          (flyspell-mode)))
   (mu4e-view-mode . (lambda() ;; try to emulate some of the eww key-bindings
                       (local-set-key (kbd "<tab>") 'shr-next-link)
                       (local-set-key (kbd "<backtab>") 'shr-previous-link)))
   (mu4e-headers-mode . (lambda ()
                          (interactive)
                          (setq mu4e-headers-fields
                                `((:human-date . 25) ;; alternatively, use :date
                                  (:flags . 6)
                                  (:from . 22)
                                  (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                                  (:size . 7))))))
  :init
  (use-package mu4e-thread-folding
    :straight (mu4e-thread-folding :type git :host github :repo "rougier/mu4e-thread-folding")
    :after mu4e
    :bind
    ((:map mu4e-headers-mode-map
           ("TAB" . mu4e-headers-toggle-at-point)
           ("C-<tab>" . mu4e-headers-toggle-fold-all))
     (:map mu4e-search-minor-mode-map
           ("S" . mu4e-kill-update-mail)))
    :custom
    (mu4e-thread-folding-default-view `folded)
    (mu4e-headers-fields '((:empty         .    2)
                           (:human-date    .   12)
                           (:flags         .    6)
                           (:mailing-list  .   10)
                           (:from          .   22)
                           (:subject       .   nil)))
    :config
    (add-to-list 'mu4e-header-info-custom
                 '(:empty . (:name "Empty"
                                   :shortname ""
                                   :function (lambda (msg) "  ")))))
  :config
  (require 'mu4e-icalendar)
  (setq mail-user-agent (mu4e-user-agent))
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)
  (defalias 'mu4e-add-attachment 'mail-add-attachment
    "I prefer the add-attachment function to begin wih mu4e so I can find it easily.")

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "Entering context gmail"))
          :leave-func (lambda () (mu4e-message "Leaving context gmail"))
          :match-func
          (lambda (msg)
            (when msg
              (string-match "gmail" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/gmail/Sent Mail")
                  (mu4e-drafts-folder . "/gmail/Drafts")
                  (mu4e-trash-folder . "/gmail/Trash")
                  (mu4e-sent-messages-behavior . sent)
                  (mu4e-compose-signature . user-full-name)
                  (user-mail-address . user-mail-address) ; Prerequisite: Set this to your email
                  (mu4e-compose-format-flowed . t)
                  (smtpmail-queue-dir . "~/Maildir/gmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "matthewzmd") ; Set to your username
                  (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t)
                  (mu4e-maildir-shortcuts . ( ("/gmail/INBOX"            . ?i)
                                              ("/gmail/Sent Mail" . ?s)
                                              ("/gmail/Trash"       . ?t)
                                              ("/gmail/All Mail"  . ?a)
                                              ("/gmail/Starred"   . ?r)
```


<a id="org7c41755"></a>

## Tramp

[Tramp](https://www.emacswiki.org/emacs/TrampMode) allows users to edit remote files seamlessly using various remote shell protocols (such as rlogin, telnet, or ssh).


<a id="org5b4e176"></a>

### Google Cloud Platform

Connect to instances on Google Cloud Platform using the format:

```text
/gssh:some-instance:/path/to/file
```

```emacs-lisp
(use-package tramp
  :straight (:type built-in)
  :defer 1
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (let ((ghcs (assoc "ghcs" tramp-methods))
        (ghcs-methods '((tramp-login-program "gh")
                        (tramp-login-args (("codespace") ("ssh") ("-c") ("%h")))
                        (tramp-remote-shell "/bin/sh")
                        (tramp-remote-shell-login ("-l"))
                        (tramp-remote-shell-args ("-c")))))
    ;; just for debugging the methods
    (if ghcs (setcdr ghcs ghcs-methods)
      (push (cons "ghcs" ghcs-methods) tramp-methods)))

  ;; provide codespace name completion for ghcs tramp method
  ;; use C-j if you use ivy to kick in host completion
```


<a id="org481ba4f"></a>

## LeetCode

[LeetCode](https://github.com/kaiwk/leetcode.el) is an Emacs client designed for interacting with LeetCode problem sets. Note that it depends on both [aio](https://github.com/skeeto/emacs-aio) and [GraphQL](https://github.com/davazp/graphql-mode) packages.

```emacs-lisp
(use-package leetcode
  :straight (leetcode :type git :host github :repo "kaiwk/leetcode.el")
  :commands (leetcode)
  :init
  (use-package graphql :defer t)
  (use-package aio :defer t)
  :custom
  (url-debug t)
  (leetcode-prefer-language "python3"))
```


<a id="orga6bed7a"></a>

## Debbugs

[Debbugs](https://elpa.gnu.org/packages/debbugs.html) is a package that grants access to the GNU Bug Tracker directly within Emacs, facilitating bug tracking processes.

```emacs-lisp
(use-package debbugs
  :commands (debbugs-gnu))
```


<a id="org119f12d"></a>

## Hacker News

A straightforward [Hacker News](https://github.com/clarete/hackernews.el) client for Emacs, enabling users to stay updated with the latest news from the platform.

```emacs-lisp
(use-package hackernews
  :commands (hackernews)
  :bind
  (("M-z h" . hackernews)
   ("M-m h" . hackernews)))
```


<a id="orgf7fdcaa"></a>

## EWW

Emacs Web Wowser (EWW) is a built-in HTML-based web browser for Emacs, allowing users to browse the web seamlessly.

```emacs-lisp
(use-package eww
  :straight (:type built-in)
  :commands (eww)
  :hook (eww-mode . (lambda ()
                      "Rename EWW's buffer so sites open in new page."
                      (rename-buffer "eww" t)))
  :config
  ;; I am using EAF-Browser instead of EWW
  (unless eaf-env-p
    (setq browse-url-browser-function 'eww-browse-url))) ; Hit & to browse url with system browser
```


<a id="org937c86a"></a>

# Miscellaneous


<a id="orgc58364d"></a>

## Chinese

This section includes packages and configurations tailored for Chinese users. Non-Chinese users can opt to disable these features by adding `:disabled` tags.


<a id="org8e5ba12"></a>

### Pyim

-   [Pyim](https://github.com/tumashu/pyim) is a versatile Chinese Pinyin input method for Emacs, enhancing text input efficiency. It leverages the [posframe](https://github.com/tumashu/posframe) package for displaying candidate options.
-   [Pyim BaseDict](https://github.com/tumashu/pyim-basedict) serves as the default dictionary for Chinese-Pyim input.

I have stopped using the recommended painless Chinese-English switching feature, as it's not very user-friendly for those needing to type in both languages simultaneously. Please use `C-\` for switching input methods if needed.

```emacs-lisp
(use-package pyim
  :init
  (use-package posframe :defer t)
  :custom
  (default-input-method "pyim")
  (pyim-default-scheme 'quanpin)
  (pyim-page-tooltip 'posframe)
  (pyim-page-length 9)
  :config
  (use-package pyim-basedict
    :after pyim
    :config (pyim-basedict-enable))
  (pyim-isearch-mode 1)
  (diminish pyim-isearch-mode "")
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  :bind
  ("M-j" . pyim-convert-string-at-point)) ; M-j 强制将光标前的拼音字符串转换为中文。
```


<a id="orgaf32515"></a>

### Youdao

[Youdao](https://github.com/xuchunyang/youdao-dictionary.el) provides an interface for leveraging Youdao's dictionary functionalities within Emacs.

```emacs-lisp
(use-package youdao-dictionary
  :commands (youdao-dictionary-search
             youdao-dictionary-search-at-point
             youdao-dictionary-search-at-point-posframe)
  :bind ("C-M-y" . youdao-dictionary-search-at-point-posframe))
```

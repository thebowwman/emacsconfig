# Doom Emacs Configuration

**Last Updated**: 2026-06-16
**Emacs Version**: 31.0.50+
**Configuration Owner**: Multi-language developer (Python, Go, TypeScript/React)

---

## 📁 Configuration Structure

```
~/.doom.d/
├── init.el              # Doom modules (what features are enabled)
├── config.el            # Main configuration and settings
├── packages.el          # Package declarations
├── modules/             # Custom module configurations
│   ├── org.el          # Org-mode capture templates, agenda
│   ├── org-roam.el     # Zettelkasten/note-taking
│   ├── vertico.el      # Completion UI
│   ├── marginalia.el   # Completion annotations
│   ├── consult.el      # Search and navigation
│   ├── company.el      # Code completion
│   ├── evil-mode.el    # Vim keybindings
│   └── modelline.el    # Statusline and performance
├── debug-adapters/       # Downloaded debug adapters (gitignored)
├── snippets/             # Custom YASnippet snippets
├── setup-debug-adapters.sh
├── CLAUDE.md
├── DAPE_DEBUGGER_GUIDE.md
├── DAPE_DEBUGGER_GUIDE.md
├── PYTHON_CONF.md
├── GO_CONF.md
├── TYPESCRIPT_CONF.md
├── ORG_MODE.md
└── BUFFER_MANAGEMENT.md
```

---

## 🎯 Quick Start

### For Python Development
→ See **[PYTHON_CONF.md](PYTHON_CONF.md)**
- LSP with basedpyright
- Auto-activate virtual environments
- Auto-imports and organize imports
- Debugging with debugpy

### For Go Development
→ See **[GO_CONF.md](GO_CONF.md)**
- LSP with gopls
- Build tags configuration
- Debugging with delve

### For TypeScript/React/Next.js
→ See **[TYPESCRIPT_CONF.md](TYPESCRIPT_CONF.md)**
- LSP with tsserver
- Auto-imports and organize imports
- Prettier formatting
- React/Next.js snippets
- Node.js debugging

### For Debugging (Any Language)
→ See **[DAPE_DEBUGGER_GUIDE.md](DAPE_DEBUGGER_GUIDE.md)**
- Dape debugger setup (replaces dap-mode)
- Universal keybindings (SPC d prefix)
- Per-project .dir-locals.el configuration
- Stepping through code

### For Org-Mode & Productivity
→ See **[ORG_MODE.md](ORG_MODE.md)**
- Capture templates (8 types)
- org-roam for notes
- Custom agenda
- GTD workflow

### For Buffer Management
→ See **[BUFFER_MANAGEMENT.md](BUFFER_MANAGEMENT.md)**
- Auto-close idle buffers
- Workspace isolation
- IBuffer organization

---

## 🔧 Core Configuration

### Enabled Doom Modules

**Completion:**
- `company` - Code completion backend
- `vertico` - Modern minibuffer completion

**UI:**
- `doom` theme
- `doom-dashboard` - Startup screen
- `modeline` - Statusline
- `treemacs` - File tree
- `workspaces` - Project isolation

**Editor:**
- `evil` (+everywhere) - Vim emulation
- `snippets` - YASnippet templates
- `format` (+onsave) - Auto-formatting

**Tools:**
- `lsp` (+peek) - Language Server Protocol
- `magit` - Git interface
- `tree-sitter` (+lang) - Better syntax parsing
- `debugger` - Dape (Debug Adapter Protocol)
- `editorconfig` - Respect .editorconfig files

**Languages:**
- Python (+lsp +tree-sitter +pyright)
- Go (+lsp)
- TypeScript (+lsp +tree-sitter)
- JavaScript (+lsp +tree-sitter)
- TSX - React support
- Rust (+lsp)
- C/C++ (+lsp)
- JSON, Data formats (TOML, CSV)
- Web (HTML/CSS)
- REST client
- Org-mode
- Markdown, YAML, Shell script

---

## ⚙️ General Settings

### Theme & UI
```elisp
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
```

### Performance Optimizations
```elisp
(setq gc-cons-threshold (* 256 1024 1024))      ; 256MB GC threshold
(setq read-process-output-max (* 4 1024 1024))  ; 4MB for LSP
(setq vc-handled-backends '(Git))                ; Only Git VCS
```

### LSP General Settings
```elisp
(setq lsp-keep-workspace-alive nil)      ; Kill workspace when last buffer closes
(setq lsp-enable-file-watchers t)
(setq lsp-file-watch-threshold 500)
```

### Evil (Vim) Settings
- Fast escape: 0.1s delay
- Granular undo (every change = separate undo)
- `C-v` paste from system clipboard in insert mode
- Cursor doesn't move back when exiting insert mode

### Custom Modules

**modules/vertico.el** - Fuzzy finding enhancements
- `C-j`/`C-k` for navigation (vim-like)
- Orderless matching (e.g., "fbf" finds "foo-bar-file")
- `SPC r v` - Repeat last completion

**modules/company.el** - Smart completions
- Auto-complete file paths (type `/`, `~/`, or `./`)
- 1 character trigger
- `C-x C-d` - Directory navigation

**modules/marginalia.el** - Enhanced annotations
- `SPC k a` - Embark act
- Shows detailed info in completion lists

**modules/consult.el** - Better search
- `M-.` - Preview files before opening
- Optimized ripgrep settings

**modules/evil-mode.el** - Vim tweaks
- See "Evil Settings" above

**modules/modelline.el** - Performance & UI
- Auto-save enabled
- Trash instead of delete
- Command history (1000 items)
- Which-key popup: 0.2s

---

## 📦 Installed Packages

### Development
- `dape` - Debug Adapter Protocol (debugging, replaces dap-mode)
- `basedpyright` - Python LSP (stricter pyright fork)
- `prettier` - Code formatting (TS/React/JSON)
- `pyvenv` - Python virtual environments
- `exec-path-from-shell` - Load shell PATH

### Productivity
- `org-roam` - Zettelkasten notes
- `org-roam-ui` - Visual graph interface
- `consult-dir` - Directory navigation

### Utilities
- `tldr` - Command help pages

### New Features (2026-06-16)
- `subword-mode` - CamelCase-aware navigation (Go/TS/Rust)
- `duplicate-dwim` - Duplicate line or region (C-S-d)
- `view-read-only` - Pager-like bindings in read-only buffers
- `undelete-frame-mode` - Undo accidental frame close
- `auto-save-visited-mode` - Auto-save files to disk (like VSCode)

---

## 🔑 Essential Keybindings

### General
- `SPC h r r` - Reload config
- `SPC f r` - Recent files
- `SPC p p` - Switch project
- `SPC TAB TAB` - Switch workspace

### Buffers
- `SPC b i` - IBuffer (organized buffer list)
- `SPC b o` - Kill other buffers
- `SPC b k` - Kill current buffer
- See **[BUFFER_MANAGEMENT.md](BUFFER_MANAGEMENT.md)** for more

### Debugging (All Languages)
- `SPC d d` - Start debugging (dape)
- `SPC d b` - Toggle breakpoint
- `SPC d n` - Step over (next)
- `SPC d s` - Step into
- `SPC d q` - Quit session
- `C-x C-a` - Dape command prefix (with repeat-mode)
- See **[DAPE_DEBUGGER_GUIDE.md](DAPE_DEBUGGER_GUIDE.md)** for complete list

### Language-Specific
See individual language documentation files

---

## 🚀 After Making Changes

### Modified `packages.el`
```bash
~/.emacs.d/bin/doom sync
```
Then restart Emacs.

### Modified `config.el` or `modules/*.el`
In Emacs:
```
SPC h r r   (doom/reload)
```
Or restart Emacs.

### Modified `init.el` (changed modules)
```bash
~/.emacs.d/bin/doom sync
```
Then restart Emacs.

---

## 🔍 Troubleshooting

### LSP not starting
1. Check language server is installed (e.g., `pyright`, `gopls`, `typescript-language-server`)
2. Verify project has proper markers (`.git`, `go.mod`, `package.json`)
3. Run `M-x lsp-doctor` for diagnostics

### DAP debugging not working
See **[DAPE_DEBUGGER_GUIDE.md](DAPE_DEBUGGER_GUIDE.md)** - Troubleshooting section

### Auto-imports not working
Check language-specific configuration files

### Buffer cleanup too aggressive
Edit `my/buffer-idle-timeout` in `config.el` (default: 10 minutes)

### PATH issues
```elisp
M-x exec-path-from-shell-initialize
```
Or restart Emacs.

---

## 📚 Documentation Index

| File | Description |
|------|-------------|
| **[DAPE_DEBUGGER_GUIDE.md](DAPE_DEBUGGER_GUIDE.md)** | Complete debugger guide for all languages |
| **[PYTHON_CONF.md](PYTHON_CONF.md)** | Python development (LSP, pyvenv, debugging) |
| **[GO_CONF.md](GO_CONF.md)** | Go development (LSP, gopls, delve) |
| **[TYPESCRIPT_CONF.md](TYPESCRIPT_CONF.md)** | TypeScript/React/Next.js/Hono development |
| **[ORG_MODE.md](ORG_MODE.md)** | Org-mode, org-roam, GTD workflow |
| **[BUFFER_MANAGEMENT.md](BUFFER_MANAGEMENT.md)** | Buffer cleanup and workspace management |

---

## 🎓 Learning Resources

- [Doom Emacs Documentation](https://github.com/doomemacs/doomemacs)
- [Dape](https://github.com/svaante/dape)
- [LSP Mode](https://emacs-lsp.github.io/lsp-mode/)
- [Org-mode Guide](https://orgmode.org/guide/)

---

## 💡 Philosophy

This configuration prioritizes:
1. **Developer productivity** - Fast completions, auto-imports, debugging
2. **Performance** - Smart GC tuning, buffer cleanup, LSP optimizations
3. **Organization** - Workspaces, IBuffer groups, modular config
4. **Maintainability** - Separate modules, clear documentation
5. **Multi-language** - Consistent experience across Python/Go/TypeScript

**Configuration Style**: Minimal duplication, single source of truth, well-commented.

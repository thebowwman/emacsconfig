# Python Development Configuration

Complete Python development setup with LSP, auto-imports, virtual environments, and debugging.

---

## 📋 Overview

**LSP Server**: Pyright (`/usr/bin/pyright`)
**Virtual Env Manager**: pyvenv
**Debugger**: debugpy (via DAP)
**Python Version**: 3.10.12 (`/usr/bin/python3`)
**Auto-features**: Imports, organize imports, type hints

### ✅ Verified Installation

```bash
Python:  /usr/bin/python3 (v3.10.12)
Pyright: /usr/bin/pyright
debugpy: Installed via pip in virtual environments
```

---

## 🔧 Configuration Details

### LSP (Language Server Protocol)

**Server**: `pyright` - Fast, feature-rich Python LSP

**Features Enabled:**
- ✅ Auto-import completions
- ✅ Type checking
- ✅ Go-to-definition
- ✅ Find references
- ✅ Rename symbol
- ✅ Organize imports on save
- ✅ Library code for better types

**Configuration:**
```elisp
(setq lsp-disabled-clients '(pylsp))  ; Disable pylsp, use pyright only
(setq lsp-pyright-auto-import-completions t)
(setq lsp-pyright-use-library-code-for-types t)
```

### Auto-Organize Imports

Runs automatically on every save:
```python
# Before save:
from typing import Dict
import sys
import os
from myapp import utils

# After save (organized):
import os
import sys
from typing import Dict

from myapp import utils
```

**Manual trigger**: `M-x lsp-organize-imports`

### Virtual Environment Support

**Auto-activation**: Automatically detects and activates `.venv` in project root

**How it works:**
```
your-project/
├── .venv/          ← Automatically detected
│   ├── bin/
│   └── lib/
├── main.py
└── requirements.txt
```

When you open `main.py`, pyvenv automatically:
1. Finds `.venv` directory
2. Activates the virtual environment
3. Updates LSP to use correct Python interpreter

**Manual activation:**
```
M-x pyvenv-activate   → Select venv path
M-x pyvenv-deactivate → Deactivate current venv
```

**Current venv indicator**: Check modeline for venv name

---

## 🐛 Debugging Setup

### Requirements

**Install debugpy in your virtual environment:**
```bash
# In your project
pip install debugpy
```

### Debug Template

**"Python :: Run Current File"**
- Debugs currently open Python file
- Automatically uses current virtual environment's Python
- No configuration needed for simple scripts

### Basic Debugging Workflow

1. **Activate virtual environment** (automatic if `.venv` exists)
2. **Set breakpoint**: `SPC m d b` on desired line
3. **Start debugging**: `SPC m d d` → Select "Python :: Run Current File"
4. **Step through**: `SPC m d n` (next), `SPC m d i` (step in)
5. **Inspect variables**: Hover or `SPC m d v`

### Advanced: Django/Flask Debugging

For web frameworks, create custom debug template in `config.el`:

```elisp
(dap-register-debug-template "Python :: Django runserver"
  (list :type "python"
        :args "-m django runserver --noreload"
        :cwd nil
        :program nil
        :request "launch"
        :name "Python :: Django runserver"))
```

### Debugging Tests

```elisp
(dap-register-debug-template "Python :: pytest current file"
  (list :type "python"
        :args "-m pytest ${file}"
        :cwd nil
        :program nil
        :request "launch"
        :name "Python :: pytest current file"))
```

---

## 🔑 Python-Specific Keybindings

### LSP Actions
| Keybinding | Command | Description |
|------------|---------|-------------|
| `g d` | `lsp-find-definition` | Go to definition |
| `g r` | `lsp-find-references` | Find all references |
| `K` | `lsp-describe-thing-at-point` | Show documentation |
| `SPC c r` | `lsp-rename` | Rename symbol across project |
| `SPC c a` | `lsp-execute-code-action` | Code actions (import, fix) |

### Debugging
All DAP keybindings work - see [DAP_DEBUGGING.md](DAP_DEBUGGING.md)

---

## 💡 Workflow Examples

### Example 1: Starting a New Project

```bash
# Create project
mkdir my-api && cd my-api

# Create virtual environment
python3 -m venv .venv
source .venv/bin/activate

# Install dependencies
pip install fastapi debugpy

# Open in Emacs
emacs main.py
```

**What happens:**
1. Emacs detects `.venv` and activates it
2. LSP starts with correct Python interpreter
3. Auto-imports work for FastAPI
4. Debugger uses venv's debugpy

### Example 2: Auto-Import Workflow

```python
# Start typing without imports
def process_data(items: L
#                       ^ Trigger completion (company-complete)
```

**Completion shows:**
```
List  (from typing)  [Auto-import]
```

**Press Enter:**
```python
from typing import List  # ← Auto-added at top

def process_data(items: List[str]):
    ...
```

### Example 3: Debugging API Endpoint

```python
# main.py
from fastapi import FastAPI

app = FastAPI()

@app.get("/users/{user_id}")
async def get_user(user_id: int):
    # Set breakpoint here ← SPC m d b
    user = await fetch_user(user_id)
    return {"user": user}
```

**Debug steps:**
1. `SPC m d b` on line 7
2. `SPC m d d` → "Python :: Run Current File"
3. In terminal: `curl http://localhost:8000/users/1`
4. Debugger pauses, inspect `user_id`

---

## 🎯 Python Interpreter Configuration

### Current Setup
```elisp
(setq lsp-pyright-python-executable-cmd "python")
(setq python-shell-interpreter "python")
```

**What this means:**
- Uses `python` from current PATH
- When venv is active, uses venv's Python
- LSP automatically detects venv's Python

### Changing Python Version

**Per-project** (recommended):
```bash
# Use Python 3.11 for this project
python3.11 -m venv .venv
```

**Globally** (change Emacs default):
```elisp
;; Add to config.el
(setq python-shell-interpreter "python3.11")
```

---

## 🚀 Performance Tips

### 1. **Exclude Large Directories**

Add to project root: `.lspignore` or `.pyproject.toml`

```toml
# pyproject.toml
[tool.pyright]
exclude = ["node_modules", "venv", ".venv", "build", "dist"]
```

### 2. **Type Stub Installation**

For better type hints:
```bash
pip install types-requests types-redis  # etc
```

### 3. **LSP Restart**

If LSP becomes slow:
```
M-x lsp-workspace-restart
```

---

## 🔍 Troubleshooting

### LSP Not Starting

**Check:**
1. Is pyright installed? `npm list -g pyright` or `pip list | grep pyright`
2. Is virtual environment activated? Check modeline
3. Is this a Python project? (has `.py` files, `setup.py`, etc.)

**Install pyright:**
```bash
# Option 1: Global (npm)
npm install -g pyright

# Option 2: Per-project (pip)
pip install pyright
```

### Auto-Imports Not Working

1. **Verify LSP is running**: `M-x lsp-describe-session`
2. **Check setting**: `M-: lsp-pyright-auto-import-completions` → should be `t`
3. **Restart LSP**: `M-x lsp-workspace-restart`

### Wrong Python Interpreter

**Check current interpreter:**
```
M-x pyvenv-workon  → Shows active venv
M-: python-shell-interpreter  → Shows interpreter path
```

**Fix:**
```
M-x pyvenv-activate → Select correct venv
M-x lsp-workspace-restart
```

### Debugger Can't Start

**Check debugpy:**
```bash
python -c "import debugpy; print(debugpy.__version__)"
```

**If missing:**
```bash
pip install debugpy
```

**Check dap-mode:**
```
M-x dap-debug  → Should show Python templates
```

### Imports Not Organizing on Save

**Verify hook is active:**
```elisp
M-: (member 'lsp-organize-imports before-save-hook)  → Should return non-nil
```

**If false, reload config:**
```
SPC h r r
```

---

## 📦 Recommended Python Packages

### Development Tools
```bash
pip install \
  debugpy \          # Debugging
  pyright \          # LSP (if using pip version)
  black \            # Formatting (if not using LSP)
  ruff \             # Fast linting
  pytest \           # Testing
  ipython            # Better REPL
```

### Type Stubs (for better completions)
```bash
pip install \
  types-requests \
  types-redis \
  types-PyYAML
```

---

## 🎓 Learning Resources

- [Pyright Documentation](https://github.com/microsoft/pyright)
- [Python LSP in Emacs](https://emacs-lsp.github.io/lsp-mode/page/lsp-pyright/)
- [debugpy GitHub](https://github.com/microsoft/debugpy)
- [pyvenv Documentation](https://github.com/jorgenschaefer/pyvenv)

---

## 🔗 Related Documentation

- [DAP_DEBUGGING.md](DAP_DEBUGGING.md) - General debugging workflow
- [CLAUDE.md](CLAUDE.md) - Main configuration index

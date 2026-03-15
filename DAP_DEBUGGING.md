# DAP Debugging Configuration

**Debug Adapter Protocol (DAP)** - Universal debugging interface for Emacs

---

## 📋 Overview

DAP mode provides debugging capabilities for multiple languages through a unified interface. Think of it as "VSCode debugging in Emacs."

**Supported Languages in This Config:**
- ✅ **Python** (via debugpy)
- ✅ **Go** (via delve)
- ✅ **TypeScript/JavaScript** (via Node.js debugger)

---

## 🔑 Universal Keybindings

These keybindings work in **all** supported languages:

### Starting & Controlling Debug Session

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC m d d` | `dap-debug` | Start debugging session |
| `SPC m d r` | `dap-continue` | Continue execution / Resume |
| `SPC m d q` | `dap-disconnect` | Quit debugging session |

### Breakpoints

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC m d b` | `dap-breakpoint-toggle` | Toggle breakpoint at current line |
| `SPC m d c` | `dap-breakpoint-condition` | Set conditional breakpoint |
| `SPC m d x` | `dap-breakpoint-delete` | Delete breakpoint at cursor |
| `SPC m d l` | `dap-breakpoint-list` | List all breakpoints |

### Stepping Through Code

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC m d n` | `dap-next` | **Step over** - Execute current line, don't enter functions |
| `SPC m d i` | `dap-step-in` | **Step into** - Enter function calls |
| `SPC m d o` | `dap-step-out` | **Step out** - Exit current function |

### Inspecting Variables

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC m d e` | `dap-eval-thing-at-point` | Evaluate expression at cursor |
| `SPC m d v` | `dap-ui-locals` | View local variables window |

---

## 🎯 Basic Debugging Workflow

### Step 1: Set Breakpoints
1. Open your source file
2. Navigate to the line where you want to pause
3. Press `SPC m d b`
4. **Visual feedback**: Red dot appears in the fringe

### Step 2: Start Debugging
1. Press `SPC m d d`
2. Select debug template:
   - Python: "Python :: Run Current File"
   - Go: "Go :: Run Current File"
   - Node/TS: "Node :: Run Current File" or "Next.js :: Debug Server"
3. **Program starts** and pauses at first breakpoint

### Step 3: Inspect & Navigate
- **Hover** over variables to see values
- Press `SPC m d v` to see all local variables
- Press `SPC m d e` to evaluate custom expressions

### Step 4: Step Through Code
- `SPC m d n` - **Most common**: Step to next line
- `SPC m d i` - Enter function call
- `SPC m d o` - Exit current function
- `SPC m d r` - Continue to next breakpoint

### Step 5: Stop Debugging
- `SPC m d q` - Quit and clean up

---

## 🔧 DAP UI Features

When debugging is active, you'll see:

### 1. **Locals Window**
Auto-opens showing:
- Local variables
- Their current values
- Variable types

### 2. **Sessions Window**
Shows active debug sessions

### 3. **Breakpoints Window**
Lists all breakpoints across files

### 4. **REPL**
Evaluate expressions in the context of the paused program

### 5. **Controls Window**
Buttons for common actions (continue, step, etc.)

---

## 📊 Debug Templates

### What Are Debug Templates?

Pre-configured debugging scenarios. Like "Run Configurations" in IDEs.

### Available Templates

#### Python
- **"Python :: Run Current File"**
  - Debugs currently open `.py` file
  - Uses debugpy

#### Go
- **"Go :: Run Current File"**
  - Debugs currently open `.go` file
  - Uses delve (dlv)

#### TypeScript/JavaScript
- **"Node :: Run Current File"**
  - Debug any `.ts` or `.js` file

- **"Next.js :: Debug Server"**
  - Debugs Next.js dev server
  - Runs `npm run dev` with debugger attached

- **"Node :: Attach to Process"**
  - Attach to already-running Node process
  - Port 9229 (default)

### How to Use Templates

1. Press `SPC m d d`
2. Use `C-j`/`C-k` to navigate
3. Press `Enter` to select
4. Debugging starts!

---

## 🎨 Visual Indicators

### Breakpoint States
- **🔴 Red dot** - Active breakpoint
- **Empty circle** - Disabled breakpoint
- **Arrow** - Current execution line (when paused)

### Buffer Decorations
- **Yellow highlight** - Current line during debug
- **Red fringe** - Breakpoint locations

---

## 💡 Pro Tips

### 1. **Conditional Breakpoints**
Stop only when a condition is true:
```
SPC m d c  → Enter condition: x > 10
```

### 2. **Evaluate Expressions**
Check values without adding print statements:
```
SPC m d e  → Type: user.email
```

### 3. **Quick Restart**
After fixing code:
```
SPC m d q  → Fix code → SPC m d d
```

### 4. **Multiple Breakpoints**
Set breakpoints in multiple files before starting

### 5. **REPL Evaluation**
Type complex expressions in the REPL for inspection

---

## 🐛 Common Debugging Scenarios

### Backend API Debugging
```
1. Set breakpoint in API handler
2. SPC m d d → Select appropriate template
3. Make HTTP request (curl, Postman, browser)
4. Debugger pauses at breakpoint
5. Inspect request data, step through logic
```

### Frontend (React) Debugging
```
1. Set breakpoint in component
2. SPC m d d → "Next.js :: Debug Server"
3. Interact with UI in browser
4. Debugger pauses when component executes
5. Inspect props, state, hooks
```

### Test Debugging
```
1. Open test file
2. Set breakpoint in failing test
3. SPC m d d → Run test with debugger
4. Step through to find issue
```

---

## 🔍 Troubleshooting

### Debugger Won't Start

**Check dependencies:**
- Python: `pip list | grep debugpy`
- Go: `which dlv`
- Node: `node --version`

**Verify PATH:**
```elisp
M-x exec-path-from-shell-initialize
```

### Breakpoints Not Hit

1. **Check file is saved** - Debugger uses saved version
2. **Verify code path is executed** - Breakpoint must be in running code
3. **Check for syntax errors** - Fix before debugging

### Can't See Variables

1. Press `SPC m d v` to open locals window
2. Check variable is in current scope
3. Try evaluating manually: `SPC m d e`

### "dap-debug not found"

1. Restart Emacs to load dap-mode
2. Check `(package! dap-mode)` in `packages.el`
3. Run `doom sync` if needed

---

## 🔗 Language-Specific Setup

For detailed setup instructions for each language:

- **Python** → See [PYTHON_CONF.md](PYTHON_CONF.md)
- **Go** → See [GO_CONF.md](GO_CONF.md)
- **TypeScript/Node** → See [TYPESCRIPT_CONF.md](TYPESCRIPT_CONF.md)

---

## 📚 Additional Resources

- [DAP Mode Documentation](https://emacs-lsp.github.io/dap-mode/)
- [Debug Adapter Protocol Spec](https://microsoft.github.io/debug-adapter-protocol/)
- [VSCode Debugging Guide](https://code.visualstudio.com/docs/editor/debugging) (concepts apply to DAP)

---

## 🎓 Learning Path

1. **Start simple**: Debug a single file with `SPC m d d`
2. **Set breakpoints**: Practice `SPC m d b` and stepping
3. **Inspect variables**: Use `SPC m d v` and `SPC m d e`
4. **Advanced**: Conditional breakpoints, REPL evaluation
5. **Complex**: Debug API requests, frontend interactions

**Time to proficiency**: ~1 hour of practice for basic debugging comfort

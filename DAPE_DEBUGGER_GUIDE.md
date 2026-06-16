# Dape Debugger Guide

Complete guide for debugging Python, Go, TypeScript/Node.js, Rust, and C/C++
in Doom Emacs using dape (Debug Adapter Protocol for Emacs).

---

## Quick Reference

### Universal Keybindings (SPC d prefix)

| Key         | Command                     | Action                    |
|-------------|-----------------------------|---------------------------|
| `SPC d d`   | `dape`                      | Start debug session       |
| `SPC d b`   | `dape-breakpoint-toggle`    | Toggle breakpoint         |
| `SPC d n`   | `dape-next`                 | Step over                 |
| `SPC d s`   | `dape-step-in`              | Step into                 |
| `SPC d o`   | `dape-step-out`             | Step out                  |
| `SPC d c`   | `dape-continue`             | Continue to next breakpoint |
| `SPC d r`   | `dape-restart`              | Restart session           |
| `SPC d q`   | `dape-quit`                 | Quit debugger             |
| `SPC d x`   | `dape-evaluate-expression`  | Evaluate expression       |
| `SPC d i`   | `dape-info`                 | Info (locals/stack/breakpoints) |
| `SPC d R`   | `dape-repl`                 | Open REPL                 |
| `SPC d w`   | `dape-watch-dwim`           | Watch variable            |
| `SPC d l`   | `dape-breakpoint-log`       | Log breakpoint            |
| `SPC d e`   | `dape-breakpoint-expression`| Conditional breakpoint    |
| `SPC d p`   | `dape-pause`                | Pause execution           |
| `SPC d D`   | `dape-disconnect-quit`      | Disconnect and quit       |

### Alternate Prefix: C-x C-a

All commands also available under `C-x C-a` with single-key suffixes:
`C-x C-a b` (breakpoint), `C-x C-a n` (next), `C-x C-a s` (step in), etc.

With `repeat-mode` enabled, after the first `C-x C-a n`, just press `n`
repeatedly to keep stepping.

---

## Standard Workflow (All Languages)

### Step 1: Set Breakpoints

1. Open your source file
2. Navigate to the line where you want execution to pause
3. Press `SPC d b` -- a red dot appears in the left fringe
4. Set as many breakpoints as needed across multiple files

### Step 2: Start the Debugger

1. Press `SPC d d`
2. In the minibuffer, select or type the adapter config name
3. Modify arguments inline if needed (e.g., `:program "src/main.ts"`)
4. Press Enter

### Step 3: Inspect State

When execution pauses at a breakpoint:

- **Locals/Stack**: Press `SPC d i` -- opens info buffer showing local
  variables, call stack, and breakpoints
- **Evaluate**: Press `SPC d x` -- type any expression to evaluate in the
  current scope
- **REPL**: Press `SPC d R` -- interactive REPL in the paused context
- **Watch**: Press `SPC d w` -- add a variable to the watch list
- **Hover**: With `eldoc-mode` enabled, hover information appears automatically

### Step 4: Navigate Code

| Action              | Key       | When to use                           |
|---------------------|-----------|---------------------------------------|
| Step over           | `SPC d n` | Execute current line, skip functions  |
| Step into           | `SPC d s` | Enter function calls                  |
| Step out            | `SPC d o` | Exit current function                 |
| Continue            | `SPC d c` | Run to next breakpoint                |
| Restart             | `SPC d r` | Restart from the beginning            |

### Step 5: Stop

- `SPC d q` -- quit the debug session cleanly

---

## Python (debugpy)

### Prerequisites

- `debugpy` installed in the project venv: `uv add debugpy --dev` or
  `pip install debugpy`
- pyvenv auto-activates the `.venv` when you open a Python file

### Debug Current File

1. Open a `.py` file
2. Set breakpoints with `SPC d b`
3. `SPC d d` --> type `debugpy` --> Enter
4. The file runs with the debugger attached, stops at breakpoints

### Debug a Module (FastAPI, Django, etc.)

1. `SPC d d` --> type `debugpy-module`
2. Override `:module` in the minibuffer:
   ```
   debugpy-module :module "uvicorn" :args ["app.main:app" "--reload"]
   ```
3. Or configure per-project in `.dir-locals.el`:
   ```elisp
   ((python-mode . ((dape-command . (debugpy-module
                                     :module "uvicorn"
                                     :args ["app.main:app" "--port" "8000"]
                                     :justMyCode nil)))))
   ```

### Debug Tests (pytest)

In the dape minibuffer:
```
debugpy-module :module "pytest" :args ["-xvs" "tests/"]
```

For a specific test file:
```
debugpy-module :module "pytest" :args ["-xvs" "tests/test_users.py"]
```

### REPL Usage

When paused at a breakpoint, press `SPC d R` to open the REPL.
Type variable names directly to see their values:
```
> user.email
'alice@example.com'
> len(users)
3
```

---

## Go (delve / dlv)

### Prerequisites

- Go installed: `/usr/local/go/bin/go`
- Delve installed: `go install github.com/go-delve/delve/cmd/dlv@latest`
- Both in PATH (configured in `config.el`)

### Debug Current Package

1. Open a `.go` file
2. Set breakpoints with `SPC d b`
3. `SPC d d` --> type `dlv` --> Enter
4. Delve builds and launches the package, stops at breakpoints

### Debug with Arguments

Override in the minibuffer:
```
dlv :args ["--config" "dev.yaml"]
```

### Debug Tests

```
dlv :mode "test" :program "."
```

For a specific test:
```
dlv :mode "test" :program "." :args ["-test.run" "TestGetUser"]
```

### Debug with Build Tags

```
dlv :buildFlags ["-tags=integration"]
```

### REPL Usage

When paused, press `SPC d R`. Delve's REPL uses Go expression syntax:
```
> user.Email
"alice@example.com"
> len(users)
3
```

---

## TypeScript / Node.js (js-debug)

### Prerequisites

- Node.js installed (via nvm, configured in `config.el`)
- `tsx` installed in the project: `npm i -D tsx`
- js-debug adapter installed at `~/.doom.d/debug-adapters/js-debug/`

### Available Adapters

| Adapter                | Use case                              |
|------------------------|---------------------------------------|
| `js-debug-node`        | Run a JS file with node               |
| `js-debug-ts-node`     | Run a TS file with ts-node            |
| `js-debug-tsx`         | Run a TS file with tsx                |
| `js-debug-node-attach` | Attach to running node process (port 9229) |
| `js-debug-chrome`      | Debug frontend in Chrome browser      |

### Debug a TypeScript Server (Hono, Express, etc.)

1. Open a `.ts` file
2. Set breakpoints with `SPC d b`
3. `SPC d d` --> type `js-debug-node` --> Enter
4. Override in minibuffer:
   ```
   js-debug-node :program "src/index.ts" :runtimeArgs ["--import" "tsx"]
   ```
5. Server starts with debugger, hit endpoints with curl to trigger breakpoints

### Per-Project Config (.dir-locals.el)

```elisp
((typescript-ts-mode . ((dape-command . (js-debug-node
                                         :program "src/index.ts"
                                         :cwd dape-cwd
                                         :runtimeArgs ["--import" "tsx"]
                                         :console "internalConsole")))))
```

With this, `SPC d d` --> Enter launches without any typing.

### Debug React Frontend (Chrome)

1. Start the dev server: `npm run dev` (e.g., Vite on port 5173)
2. Open a `.tsx` file, set breakpoints
3. `SPC d d` --> type `js-debug-chrome`
4. Override URL:
   ```
   js-debug-chrome :url "http://localhost:5173"
   ```
5. Chrome opens, interact with the app, breakpoints fire in Emacs

### Attach to Running Process

If your app is already running with `--inspect`:
```bash
node --inspect src/index.js
```

Then in Emacs:
```
js-debug-node-attach :port 9229
```

### Debug Tests

```
js-debug-node :program "node_modules/.bin/vitest" :args ["run" "--reporter" "verbose"]
```

### REPL Usage

When paused, press `SPC d R`. Use JavaScript/TypeScript expressions:
```
> user.email
'alice@example.com'
> users.length
3
```

---

## Rust (lldb-dap)

### Prerequisites

- `lldb-dap-20` installed: `sudo apt install lldb-20`
- Cargo/rustc installed (via rustup)
- Project built with debug symbols (default for `cargo build`)

### Available Adapters

| Adapter           | Use case                                         |
|-------------------|--------------------------------------------------|
| `rust-lldb`       | Auto-builds with `cargo build`, detects binary   |
| `rust-lldb-test`  | Auto-builds tests, prompts for test binary       |
| `lldb-dap`        | Manual -- you specify the binary path            |
| `gdb`             | Alternative using GDB 14.1+                      |

### Debug a Binary (Recommended)

1. Open a `.rs` file
2. Set breakpoints with `SPC d b`
3. `SPC d d` --> type `rust-lldb` --> Enter
4. Cargo builds automatically, detects binary from `Cargo.toml`, launches

### Debug Tests

1. Set breakpoints in test functions
2. `SPC d d` --> type `rust-lldb-test` --> Enter
3. Cargo builds tests, prompts you to select the test binary
4. Tests run with `--nocapture`, stops at breakpoints

### Debug with Manual Binary

If you need to specify the binary directly:
```
lldb-dap :program "target/debug/my-app" :args ["--verbose"]
```

### REPL Usage

When paused, press `SPC d R`. lldb-dap has a proper expression evaluator:
```
> result
42
> user.name
"Alice"
> vec.len()
3
```

This is a full expression REPL (unlike GDB which requires `p variable`).

---

## C / C++ (lldb-dap or gdb)

### Prerequisites

- `lldb-dap-20`: `sudo apt install lldb-20`
- Or GDB 14.1+: already installed (GDB 15.1)
- Compile with debug symbols: `gcc -g -o myapp main.c`

### Debug with lldb-dap

1. Compile: `gcc -g -o myapp main.c`
2. Open the source file, set breakpoints
3. `SPC d d` --> type `lldb-dap`
4. Set `:program "myapp"`

### Debug with GDB

```
gdb :program "myapp" :stopAtBeginningOfMainSubprogram t
```

Note: GDB requires `stopAtBeginningOfMainSubprogram t` to reliably hit
breakpoints (a GDB DAP quirk).

---

## Per-Project Configuration (.dir-locals.el)

Instead of typing adapter configs every time, create a `.dir-locals.el`
in your project root:

### Python (FastAPI)

```elisp
((python-mode . ((dape-command . (debugpy-module
                                  :module "uvicorn"
                                  :args ["app.main:app" "--port" "8000"]
                                  :justMyCode nil
                                  :console "integratedTerminal")))))
```

### TypeScript (Hono/Express)

```elisp
((typescript-ts-mode . ((dape-command . (js-debug-node
                                         :program "src/index.ts"
                                         :cwd dape-cwd
                                         :runtimeArgs ["--import" "tsx"]
                                         :console "internalConsole")))))
```

### LSP File Watch Exclusions (combine with above)

```elisp
((nil . ((lsp-file-watch-ignored-directories . ("[/\\\\]node_modules\\'"
                                                "[/\\\\]\\.venv\\'"
                                                "[/\\\\]\\.git\\'"
                                                "[/\\\\]dist\\'")))))
```

After creating/modifying `.dir-locals.el`, revert the buffer (`SPC b r`)
or restart Emacs to pick up the changes.

---

## Breakpoint Types

| Type            | Key       | Description                           |
|-----------------|-----------|---------------------------------------|
| Regular         | `SPC d b` | Pause execution at this line          |
| Conditional     | `SPC d e` | Pause only when expression is true    |
| Log             | `SPC d l` | Print a message without pausing       |
| Hit count       |`C-x C-a h`| Pause after N hits                    |

### Conditional Breakpoint Example

`SPC d e` on a line, then enter: `user.id == "42"`

The debugger only pauses when the condition evaluates to true.

### Log Breakpoint Example

`SPC d l` on a line, then enter: `User email: {user.email}`

Prints to the debug console without stopping execution.

---

## Troubleshooting

### Debugger won't start

1. Check the adapter is installed:
   - Python: `.venv/bin/python -c "import debugpy"`
   - Go: `which dlv`
   - JS/TS: `ls ~/.doom.d/debug-adapters/js-debug/src/dapDebugServer.js`
   - Rust: `which lldb-dap-20`

2. Check `*dape-repl*` buffer for error messages

3. Enable debug logging: `(setq dape-debug t)` then check
   `*dape-connection events*` buffer

### Breakpoints not hit

1. Save the file before debugging (auto-save handles this)
2. Ensure code path is actually executed
3. For Rust/C: verify binary has debug symbols (`file target/debug/myapp`
   should show `with debug_info`)
4. For GDB: use `:stopAtBeginningOfMainSubprogram t`

### "Unable to locate" errors

- Python `basedpyright-langserver`: venv not activated -- open a Python file
  first to trigger pyvenv
- Node `tsx`/`ts-node`: not in global PATH, use `node --import tsx` instead
- Go `go`: ensure `/usr/local/go/bin` is in exec-path

### REPL shows command errors

- **GDB**: Use `p variable` or `print variable` (GDB command syntax)
- **lldb-dap**: Type variable names directly (expression evaluator)
- **debugpy**: Type Python expressions directly
- **dlv**: Type Go expressions directly

---

## Resources

- [Dape GitHub](https://github.com/svaante/dape)
- [Debug Adapter Protocol Spec](https://microsoft.github.io/debug-adapter-protocol/)
- [debugpy options](https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings)
- [delve documentation](https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md)
- [js-debug options](https://github.com/microsoft/vscode-js-debug/blob/main/OPTIONS.md)

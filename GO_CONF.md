# Go Development Configuration

Complete Go development setup with LSP, gopls, and delve debugging.

---

## 📋 Overview

**LSP Server**: gopls v0.21.1 (official Go language server)
**Debugger**: delve v1.26.1 (dlv)
**Build Tags**: `-tags=integration` enabled by default
**Go Version**: 1.26.1

### ✅ Verified Installation

```bash
Go:    ~/go/bin/go (v1.26.1)
delve: ~/go/bin/dlv (v1.26.1)
gopls: ~/go/bin/gopls (v0.21.1)
```

**Important**: Go tools are in `~/go/bin/` - make sure this is in your PATH:
```bash
export PATH=$PATH:$HOME/go/bin
```

Then reload Emacs PATH:
```elisp
M-x exec-path-from-shell-initialize
```

---

## 🔧 Configuration Details

### LSP (Language Server Protocol)

**Server**: `gopls` - Official Go language server

**Features Enabled:**
- ✅ Auto-completions
- ✅ Go-to-definition
- ✅ Find references
- ✅ Refactoring support
- ✅ Code actions (add imports, generate stubs)
- ✅ Inline type hints
- ✅ Build tag support

**Special Configuration:**
```elisp
(setq lsp-go-build-flags ["-tags=integration"])
```

**Why build tags?** Allows LSP to analyze code in files with `// +build integration`

### Build Tags Explained

**Without build tags:**
```go
// user_test.go
// +build integration

// ← LSP ignores this file (grayed out)
```

**With `-tags=integration`:**
```go
// user_test.go
// +build integration

// ← LSP analyzes this file (full support)
```

**Change build tags:**
Edit `config.el`:
```elisp
(setq lsp-go-build-flags ["-tags=integration,e2e,custom"])
```

---

## 🐛 Debugging Setup

### Requirements

✅ **All tools are installed and verified:**

```bash
~/go/bin/go version
# Output: go version go1.26.1 linux/amd64

~/go/bin/dlv version
# Output: Delve Debugger Version: 1.26.1

~/go/bin/gopls version
# Output: golang.org/x/tools/gopls v0.21.1
```

**Ensure ~/go/bin is in PATH** for Emacs to find these tools.

### Debug Template

**"Go :: Run Current File"**
- Debugs currently open `.go` file with `main()` function
- Uses delve under the hood
- Respects build tags

### Basic Debugging Workflow

1. **Open Go file** with `main()` function
2. **Set breakpoint**: `SPC m d b` on desired line
3. **Start debugging**: `SPC m d d` → Select "Go :: Run Current File"
4. **Step through**: `SPC m d n` (next), `SPC m d i` (step in)
5. **Inspect variables**: Hover or `SPC m d v`

### Debugging Tests

Create custom template in `config.el`:

```elisp
(dap-register-debug-template "Go :: Test Current Function"
  (list :type "go"
        :request "launch"
        :name "Go :: Test Current Function"
        :mode "test"
        :program "${workspaceFolder}"
        :args "-test.run MyTestFunction"))
```

### Debugging with Build Tags

If debugging integration tests:

```elisp
(dap-register-debug-template "Go :: Run with Integration Tag"
  (list :type "go"
        :request "launch"
        :name "Go :: Run with Integration Tag"
        :mode "debug"
        :program nil
        :buildFlags "-tags=integration"))
```

---

## 🔑 Go-Specific Keybindings

### LSP Actions
| Keybinding | Command | Description |
|------------|---------|-------------|
| `g d` | `lsp-find-definition` | Go to definition |
| `g r` | `lsp-find-references` | Find all references |
| `g i` | `lsp-find-implementation` | Go to interface implementation |
| `K` | `lsp-describe-thing-at-point` | Show documentation |
| `SPC c r` | `lsp-rename` | Rename symbol across project |
| `SPC c a` | `lsp-execute-code-action` | Code actions (add imports, generate) |

### Debugging
All DAP keybindings work - see [DAP_DEBUGGING.md](DAP_DEBUGGING.md)

---

## 💡 Workflow Examples

### Example 1: Starting a New Project

```bash
# Create project
mkdir my-api && cd my-api

# Initialize Go module
go mod init github.com/user/my-api

# Create main file
cat > main.go <<EOF
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
EOF

# Open in Emacs
emacs main.go
```

**What happens:**
1. LSP auto-starts (detects `go.mod`)
2. Completions work immediately
3. Build tags applied
4. Ready to debug!

### Example 2: Auto-Import Workflow

```go
package main

func main() {
    // Start typing without imports
    fmt.Pri
    //     ^ Trigger completion
}
```

**Completion shows:**
```
Printf  (func in fmt)  [Auto-import]
```

**Press Enter:**
```go
package main

import "fmt"  // ← Auto-added

func main() {
    fmt.Printf
}
```

### Example 3: Debugging API Server

```go
// main.go
package main

import (
    "net/http"
    "github.com/gin-gonic/gin"
)

func main() {
    r := gin.Default()

    r.GET("/users/:id", func(c *gin.Context) {
        id := c.Param("id")
        // Set breakpoint here ← SPC m d b
        user := fetchUser(id)
        c.JSON(200, user)
    })

    r.Run(":8080")
}
```

**Debug steps:**
1. `SPC m d b` on line with `user := fetchUser(id)`
2. `SPC m d d` → "Go :: Run Current File"
3. In terminal: `curl http://localhost:8080/users/123`
4. Debugger pauses, inspect `id` and `user`

### Example 4: Testing with Build Tags

```go
// user_repository_test.go
// +build integration

package repository

import "testing"

func TestUserRepository_Integration(t *testing.T) {
    // This test needs real database
    // Set breakpoint here ← SPC m d b
    repo := NewUserRepository(testDB)
    user, err := repo.GetByID(1)
    // ...
}
```

**With build tags configured**, LSP fully supports this file!

---

## 🎯 Go Tools Integration

### Automatic Formatting

Go files are formatted on save using `gofmt` (via LSP).

**Additional formatters:**
```bash
# Install goimports (better than gofmt)
go install golang.org/x/tools/cmd/goimports@latest

# Configure LSP to use it
(setq lsp-go-format-tool "goimports")
```

### Code Generation

**LSP code actions** (`SPC c a`) can:
- Generate interface stubs
- Fill struct fields
- Add missing imports
- Extract to function
- Generate tests

**Example:**
```go
type Handler interface {
    HandleRequest(req Request) Response
}

type MyHandler struct {}  // ← Cursor here, SPC c a
                          // → "Implement interface Handler"
```

---

## 🚀 Performance Tips

### 1. **Exclude Vendor Directories**

LSP scans everything by default. Exclude large directories:

```bash
# Create .gopls file in project root
cat > .gopls <<EOF
{
  "directoryFilters": [
    "-vendor",
    "-node_modules",
    "-**/testdata"
  ]
}
EOF
```

### 2. **Increase LSP Timeout**

For large codebases:
```elisp
(setq lsp-gopls-server-args '("-rpc.trace"))
```

### 3. **gopls Memory Limit**

If gopls uses too much memory:
```bash
# Set GOMEMLIMIT environment variable
export GOMEMLIMIT=2GiB
```

---

## 🔍 Troubleshooting

### LSP Not Starting

**Check:**
1. Is gopls installed? `which gopls`
2. Is this a Go module? (has `go.mod`)
3. Is Go in PATH? `which go`

**Install gopls:**
```bash
go install golang.org/x/tools/gopls@latest
```

**Check PATH:**
```elisp
M-x exec-path-from-shell-initialize
```

### Build Tag Files Grayed Out

**Verify build flags:**
```elisp
M-: lsp-go-build-flags  → Should show ["tags=integration"]
```

**If not set:**
1. Check `config.el` has the setting
2. Restart LSP: `M-x lsp-workspace-restart`

### Delve Not Found

**Error**: `/home/user/go/bin/dlv: No such file or directory`

**Fix:**
```bash
# Install delve
go install github.com/go-delve/delve/cmd/dlv@latest

# Verify installation
dlv version

# Check PATH
echo $PATH | grep go/bin
```

**If PATH is wrong:**
```bash
# Add to ~/.bashrc
export PATH=$PATH:$HOME/.local/go/bin:$HOME/go/bin

# Reload
source ~/.bashrc
```

**In Emacs:**
```elisp
M-x exec-path-from-shell-initialize
```

### Can't Debug, "Program Not Found"

**Issue**: Debug template can't find program to run

**Solutions:**

1. **Ensure file has `main()` function**
   ```go
   package main  // ← Must be package main

   func main() {  // ← Must have main()
       // ...
   }
   ```

2. **Use correct debug template**
   - "Go :: Run Current File" for `main` packages
   - "Go :: Test" for test files

3. **Check working directory**
   - DAP runs from project root
   - Ensure project has `go.mod`

### Slow Completions

**Likely causes:**
1. Large vendor directory (exclude it)
2. gopls analyzing too many files
3. gopls cache corruption

**Solutions:**
```bash
# Clear gopls cache
rm -rf ~/.cache/gopls

# Restart LSP in Emacs
M-x lsp-workspace-restart
```

---

## 📦 Recommended Go Tools

### Development Tools
```bash
# LSP and formatters
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# Debugging
go install github.com/go-delve/delve/cmd/dlv@latest

# Linting
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

# Testing
go install gotest.tools/gotestsum@latest
```

### Project Structure Tools
```bash
# Dependency management
go install github.com/go-mod-tools/mod-upgrade@latest

# API development
go install github.com/swaggo/swag/cmd/swag@latest  # Swagger docs
```

---

## 🎓 Learning Resources

- [gopls Documentation](https://github.com/golang/tools/tree/master/gopls)
- [Delve Debugger](https://github.com/go-delve/delve)
- [Go LSP in Emacs](https://emacs-lsp.github.io/lsp-mode/page/lsp-gopls/)
- [Build Tags Guide](https://www.digitalocean.com/community/tutorials/customizing-go-binaries-with-build-tags)

---

## 🔗 Related Documentation

- [DAP_DEBUGGING.md](DAP_DEBUGGING.md) - General debugging workflow
- [CLAUDE.md](CLAUDE.md) - Main configuration index

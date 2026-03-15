# Buffer Management

Strategies and tools for managing buffers efficiently in long Emacs sessions.

---

## 📋 The Buffer Problem

**Common scenario:**
- Open 50+ files during development
- Many become "stale" (not actively used)
- Buffer list becomes cluttered
- Harder to find what you need
- Memory usage increases

**Our solution:** **3-tier buffer management**
1. Automatic cleanup (passive)
2. Workspace isolation (organizational)
3. Manual tools (when needed)

---

## 🤖 Tier 1: Automatic Cleanup

### Auto-Close Idle Saved Buffers

**Configuration:**
```elisp
(defvar my/buffer-idle-timeout (* 10 60))  ; 10 minutes
```

**How it works:**
- Checks every 2 minutes
- Closes buffers that are:
  - ✅ File-backed (actual files, not special buffers)
  - ✅ **Saved** (no unsaved changes)
  - ✅ Inactive for 10+ minutes
  - ✅ Not currently visible in any window

**Never closes:**
- Unsaved buffers (your work is safe!)
- Special buffers (`*scratch*`, `*Messages*`, `*dashboard*`)
- Magit/Git buffers
- Org Agenda/Src edit buffers
- Any buffer currently visible

### Example Timeline

```
09:00 - Open main.go, edit, save
09:05 - Open utils.go, edit, save
09:10 - Open api.go, start editing
        ↓
09:10 - main.go auto-closes (10 min idle, saved)
09:15 - utils.go auto-closes (10 min idle, saved)
09:20 - api.go STAYS OPEN (still editing, unsaved)
```

### Adjusting Timeout

Edit `config.el`:
```elisp
;; For 5 minutes
(defvar my/buffer-idle-timeout (* 5 60))

;; For 15 minutes
(defvar my/buffer-idle-timeout (* 15 60))
```

Reload: `SPC h r r`

### Reopening Auto-Closed Files

**Recentf** remembers recently closed files:

```
SPC f r  → Shows last 200 files
         → Select file → Reopens instantly
```

Files stay in recentf for days/weeks!

---

## 🗂️ Tier 2: Workspace Isolation

### What Are Workspaces?

**Think:** Virtual desktops for Emacs

Each workspace has:
- **Own buffer list** (isolated)
- **Own window layout**
- **Own working directory**

**Result:** Clean separation between projects

### Workspace Keybindings

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC TAB n` | New workspace | Create fresh workspace |
| `SPC TAB TAB` | Switch workspace | Jump between workspaces |
| `SPC TAB r` | Rename workspace | Give it a meaningful name |
| `SPC TAB d` | Delete workspace | Close and cleanup |
| `SPC TAB .` | Load workspace | Restore saved workspace |
| `SPC TAB s` | Save workspace | Save for later |

### Workflow Example

#### Morning: Backend Work
```
SPC TAB n  → Create "backend-api"
           → Open Go files
           → Work on API
           → Buffer list: api.go, db.go, handlers.go
```

#### Afternoon: Frontend Work
```
SPC TAB n  → Create "frontend-ui"
           → Open React files
           → Work on UI
           → Buffer list: App.tsx, Button.tsx, api.ts
           → (Backend buffers completely hidden!)
```

#### Evening: Cleanup
```
SPC TAB d  → Delete "frontend-ui"
           → All UI buffers closed
           → Switch to "backend-api"
           → Continue tomorrow
```

### Benefits

✅ **Mental clarity** - Only see relevant buffers
✅ **Faster navigation** - Smaller buffer list
✅ **Context switching** - Project boundaries
✅ **Quick cleanup** - Delete entire workspace

---

## 🛠️ Tier 3: Manual Tools

### IBuffer - Power User Buffer Manager

**Open**: `SPC b i` or `SPC b I`

**What you see:**
```
[ Org ]
  org-mode       agenda.org              256B
  org-mode       ideas.org               1.2K

[ Programming ]
  python-mode    main.py                 4.5K
  go-mode        server.go               8.1K
  typescript-tsx Button.tsx              2.3K

[ Dired ]
  dired-mode     ~/projects/

[ Magit ]
  magit-status   magit: my-project

[ Temp ]
  *compilation*
  *lsp-log*
```

**Auto-grouped by:**
- **Org** - All org-mode files
- **Programming** - Python, Go, TypeScript, Rust, C/C++
- **Dired** - Directory browsers
- **Magit** - Git buffers
- **Emacs** - Core buffers (scratch, messages)
- **Help** - Documentation
- **Temp** - Temporary buffers

### IBuffer Keybindings

**Navigation:**
| Key | Action | Description |
|-----|--------|-------------|
| `j/k` | Move | Navigate buffer list |
| `RET` | Open | Switch to buffer |
| `g` | Refresh | Reload list |

**Selection:**
| Key | Action | Description |
|-----|--------|-------------|
| `m` | Mark | Mark buffer for action |
| `u` | Unmark | Unmark buffer |
| `U` | Unmark all | Clear all marks |
| `t` | Toggle marks | Invert selection |
| `* /` | Mark by regexp | Mark matching names |

**Actions:**
| Key | Action | Description |
|-----|--------|-------------|
| `d` | Mark for deletion | Mark to kill |
| `x` | Execute | Kill marked buffers |
| `S` | Save | Save marked buffers |
| `D` | Delete immediately | Skip marking |

**Filtering:**
| Key | Action | Description |
|-----|--------|-------------|
| `/ m` | Filter by mode | Show only python-mode, etc. |
| `/ n` | Filter by name | Show matching names |
| `/ /` | Clear filters | Show all |

### IBuffer Workflow Example

**Scenario**: Clean up after debugging session

1. `SPC b i` - Open ibuffer
2. Navigate to "Temp" group
3. `m` on `*compilation*` - Mark
4. `m` on `*lsp-log*` - Mark
5. `m` on `*dap-server-log*` - Mark
6. `x` - Kill all marked buffers
7. `q` - Close ibuffer

**Result**: All temporary buffers gone, project files remain

---

## 🎯 Quick Cleanup Commands

### One-Shot Cleanup

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC b k` | Kill current | Close active buffer |
| `SPC b K` | Kill ALL buffers | Nuclear option |
| `SPC b o` | Kill other buffers | Keep only current |
| `SPC b O` | Kill other buffers (workspace) | Doom's version |
| `SPC b C` | Kill buried buffers | Clean background buffers |
| `SPC b P` | Kill project buffers | Close all in project |

### When to Use Each

**`SPC b o`** - "Focus mode"
```
Working on main.py, want to focus
SPC b o → Only main.py remains
```

**`SPC b C`** - "Background cleanup"
```
Lots of hidden buffers (not visible)
SPC b C → Cleans background, keeps visible
```

**`SPC b P`** - "Done with project"
```
Finished working on project
SPC b P → Closes all project buffers
Can reopen project later
```

**`SPC b K`** - "Fresh start"
```
Too many buffers, start over
SPC b K → Everything gone
Uses recentf to reopen needed files
```

---

## 📊 Buffer Management Strategies

### Strategy 1: Workspace Per Project (Recommended)

**Best for:** Multi-project developers

```
Morning:
  SPC TAB n → "backend"
  Open backend files
  Work all day
  Auto-cleanup handles idle buffers

Next morning:
  SPC TAB TAB → Switch to "backend"
  Recent files still there
  Continue working
```

**Benefits:**
- Clear project boundaries
- Auto-cleanup handles idle files
- Easy context switching
- Delete workspace = instant cleanup

### Strategy 2: Regular Manual Cleanup

**Best for:** Single-project focus

```
Every hour:
  SPC b i → Open ibuffer
  Check "Temp" group
  Mark unnecessary buffers
  x → Delete

End of day:
  SPC b C → Kill buried buffers
  Fresh start tomorrow
```

**Benefits:**
- More control
- Understand what's open
- Manual review process

### Strategy 3: Mostly Automatic (Default)

**Best for:** Trust the system

```
Just work:
  Open files as needed
  Save frequently
  Auto-cleanup handles the rest

Occasionally:
  SPC f r → Reopen needed files
  SPC b C → Quick cleanup if needed
```

**Benefits:**
- Hands-off
- Less mental overhead
- Save frequently = safe cleanup

### Strategy 4: Aggressive Manual

**Best for:** Minimalists

```
Constantly:
  SPC b o → Focus on current file
  Open next file
  SPC b o → Focus again
  Repeat

Result:
  Only ever 1 file open
  Maximum focus
  Use recentf heavily
```

**Benefits:**
- Absolute minimal buffers
- Maximum focus
- Forces intentional file opening

---

## 🔍 Finding Buffers

### Switch Buffer Commands

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC b b` | Switch buffer | List all buffers |
| `SPC b B` | Switch buffer (all workspaces) | Across workspaces |
| `SPC ,` | Switch buffer (project) | Current project only |
| `SPC <` | Switch buffer (workspace) | Current workspace |

### Search & Filter

**Vertico completion** shows:
```
main.py        (python-mode)
server.go      (go-mode)
Button.tsx     (typescript-tsx-mode)
```

**Type to filter:**
```
main  → Shows main.py, main.go, main.tsx
.tsx  → Shows all .tsx files
go    → Shows .go files and go-mode buffers
```

**Orderless matching:**
```
py mn  → Matches "python main" → main.py
```

---

## 💡 Pro Tips

### 1. **Name Workspaces Meaningfully**

**Bad:**
```
workspace-1
workspace-2
```

**Good:**
```
backend-api
frontend-react
docs-writing
```

Use: `SPC TAB r` to rename

### 2. **Save Frequently**

Auto-close only works on **saved** buffers.

**Enable auto-save:**
```elisp
;; Already in config.el
(setq auto-save-default t)
```

Or manually: `SPC f s` often

### 3. **Trust Recentf**

Don't fear auto-close! `SPC f r` always has your files.

**Recentf settings:**
```elisp
(setq recentf-max-saved-items 200)  ; Last 200 files
(setq recentf-max-menu-items 50)    ; Show 50 in menu
```

### 4. **Use Project Navigation**

Instead of buffer list, use project files:

```
SPC p f  → Find file in project
SPC SPC  → Find any file (smart)
```

### 5. **Monitor Buffer Count**

Check modeline occasionally:
```
[workspace: backend] [15 buffers]
                     ↑ If this is >30, cleanup!
```

### 6. **Periodic Deep Clean**

Weekly:
```
SPC b i       → Open ibuffer
* /           → Mark all by regexp: .*
d             → Mark for deletion
u u u         → Unmark files you want
x             → Execute
```

---

## 🔍 Troubleshooting

### Auto-Cleanup Too Aggressive

**Symptom**: Files I need keep getting closed

**Solutions:**

1. **Save more frequently** (only saved files close)
2. **Increase timeout**:
   ```elisp
   (setq my/buffer-idle-timeout (* 20 60))  ; 20 minutes
   ```
3. **Add to never-kill patterns**:
   ```elisp
   (add-to-list 'my/buffer-never-kill-patterns "important-file.go")
   ```

### Auto-Cleanup Not Working

**Check:**
```elisp
M-: my/buffer-idle-timeout  → Should show 600 (10 min)
M-x list-timers             → Should show kill-idle-buffers timer
```

**Restart:**
```
SPC h r r
```

### Can't Find Closed Buffer

**Use recentf:**
```
SPC f r  → Shows recent files
```

**Still can't find?**
```
SPC SPC  → Search all files in project
```

### IBuffer Groups Not Showing

**Reload ibuffer:**
```
g  (in ibuffer)
```

**Check configuration:**
```elisp
M-: ibuffer-saved-filter-groups  → Should show groups
```

---

## 🎓 Learning Path

**Week 1**: Learn auto-cleanup
- Let it run
- Watch what gets closed
- Use `SPC f r` to reopen

**Week 2**: Try workspaces
- Create 2-3 workspaces
- Switch between them
- Feel the benefit

**Week 3**: Master IBuffer
- Open `SPC b i` daily
- Practice marking and deletion
- Use filters

**Week 4**: Optimize your strategy
- Choose strategy that fits
- Tweak timeout if needed
- Build muscle memory

---

## 🔗 Related Documentation

- [CLAUDE.md](CLAUDE.md) - Main configuration index
- [ORG_MODE.md](ORG_MODE.md) - Managing org buffers specifically

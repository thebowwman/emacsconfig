# Org-Mode Configuration

Complete org-mode setup for productivity, note-taking, and GTD (Getting Things Done).

---

## 📋 Overview

**Org Directory**: `~/org/`
**Features**: Capture templates, agenda, org-roam (zettelkasten), auto-clock, habits
**Philosophy**: GTD + Zettelkasten for maximum productivity

---

## 📁 Org Directory Structure

```
~/org/
├── inbox.org         # Captured TODOs (to be processed)
├── calendar.org      # Events and deadlines
├── projects.org      # Active projects
├── ideas.org         # Ideas and someday/maybe
├── notes.org         # General notes
├── bookmarks.org     # Web bookmarks with tags
├── contacts.org      # Contact information
├── done.org          # Archived completed items
├── daily/            # Daily TODO files
│   ├── 2026-03-15.org
│   ├── 2026-03-16.org
│   └── ...
└── roam/             # Org-roam knowledge base
    └── ...
```

---

## 🎯 Capture Templates

Press `SPC X` or `SPC n n` to open capture menu.

### Quick Reference

| Key | Template | Destination | Use Case |
|-----|----------|-------------|----------|
| `t` | Todo | inbox.org | Quick task capture |
| `e` | Event | calendar.org | Scheduled event |
| `d` | Deadline | calendar.org | Task with due date |
| `p` | Project | projects.org | New project |
| `i` | Idea | ideas.org | Brainstorming |
| `b` | Bookmark | bookmarks.org | Save interesting links |
| `c` | Contact | contacts.org | Save contact info |
| `n` | Note | notes.org | General note |
| `D` | Daily TODO | daily/YYYY-MM-DD.org | Daily planning |

---

## 📝 Capture Templates Detailed

### 1. **Todo** (`t`)
**Quick task capture with metadata**

```org
* TODO Fix login bug
:PROPERTIES:
:CREATED: [2026-03-15 Sat 14:30]
:CAPTURED: [[file:~/project/auth.go::42][auth.go::42]]
:END:
```

**Captured info:**
- Creation timestamp
- Where you were when capturing (file + line)

### 2. **Event** (`e`)
**Scheduled events**

```org
* Team Standup
<2026-03-16 Sun 09:00>
:PROPERTIES:
:CREATED: [2026-03-15 Sat 14:30]
:CAPTURED: [[file:~/org/inbox.org][inbox.org]]
:CONTACT: [[id:abc123][John Doe]]
:END:
```

**Features:**
- Scheduled date/time
- Linked contact (optional)
- Shows in agenda

### 3. **Deadline** (`d`)
**Task with due date**

```org
* TODO Submit quarterly report
DEADLINE: <2026-03-20 Wed>
:PROPERTIES:
:CREATED: [2026-03-15 Sat 14:30]
:END:
```

**Agenda behavior:**
- Shows countdown: "In 5 days"
- Warns as deadline approaches

### 4. **Project** (`p`)
**Project with first task**

```org
* PROJ Redesign Homepage
:PROPERTIES:
:CREATED: [2026-03-15 Sat 14:30]
:END:
** TODO Define requirements
```

**Use for:**
- Multi-step initiatives
- Ongoing work
- Complex tasks

### 5. **Idea** (`i`)
**Quick idea capture**

```org
** IDEA Use WebAssembly for performance
:PROPERTIES:
:CREATED: [2026-03-15 Sat 14:30]
:END:
```

**Review later:**
- Weekly review: process ideas
- Move to projects or delete

### 6. **Bookmark** (`b`)
**Save links with tags**

```org
** [[https://example.com][Interesting Article]]
:PROPERTIES:
:CREATED: [2026-03-15 Sat 14:30]
:TAGS: programming, typescript, performance
:END:

```

**Features:**
- Tag-based organization
- Auto-complete existing tags
- Search by tag later

### 7. **Contact** (`c`)
**Contact information**

```org
* John Doe
:PROPERTIES:
:CREATED: [2026-03-15 Sat 14:30]
:EMAIL: john@example.com
:PHONE: +1-555-0123
:BIRTHDAY: <1990-05-15 +1y>
:LOCATION: New York, NY
:LAST_CONTACTED: [2026-03-15 Sat]
:END:
*** Communications
*** Notes
```

**Features:**
- Birthday reminders (yearly)
- Communication history
- Reference in other captures

### 8. **Daily TODO** (`D`)
**Daily planning**

```org
#+title: March 15, 2026

* TODO Morning review
* TODO Work on feature X
* TODO Team meeting 2pm
```

**Use for:**
- Daily planning
- Time-blocking
- End-of-day review

---

## 📅 Org Agenda

**Open agenda**: `SPC o a` or `M-x org-agenda`

### Custom Dashboard View

Press `d` in agenda for "Dashboard" view:

#### 1. **Highest Priority**
Tasks marked `[#A]` priority.

```org
HIGHEST PRIORITY
  ☐ Fix critical bug           :work:
  ☐ Submit tax documents        :personal:
```

#### 2. **Today's Schedule**
Events and deadlines for today.

```org
TODAY'S SCHEDULE
  09:00 Team standup
  14:00 Client meeting
  ━━━━━ NOW  ← Current time indicator
  16:00 Code review
```

#### 3. **All TODO**
All open TODOs sorted by priority.

```org
ALL TODO
  High priority tasks first
  Then medium priority
  Then normal tasks
```

**Excludes:**
- Habits (separate tracking)
- Completed items
- Scheduled for future dates (unless overdue)

### Agenda Keybindings

| Key | Action | Description |
|-----|--------|-------------|
| `d` | Dashboard | Custom dashboard view |
| `t` | Mark TODO | Cycle through TODO states |
| `q` | Quit | Close agenda |
| `r` | Refresh | Reload agenda |
| `j/k` | Navigate | Move up/down |
| `RET` | Goto | Jump to item |
| `I` | Clock in | Start tracking time |
| `O` | Clock out | Stop tracking time |

---

## ⏰ Auto-Clock In/Out

**Automatic time tracking** based on TODO state.

### How It Works

**When you change state to STRT:**
```org
* TODO Write documentation
  ↓ (Change to STRT)
* STRT Write documentation
  CLOCK: [2026-03-15 Sat 14:30]  ← Auto-clocked in!
```

**When you change STRT to anything else:**
```org
* STRT Write documentation
  CLOCK: [2026-03-15 Sat 14:30]--[2026-03-15 Sat 15:45] =>  1:15
  ↓ (Change to DONE)
* DONE Write documentation  ← Auto-clocked out!
```

### Clock Reports

**View time spent:**
```org
#+BEGIN: clocktable :maxlevel 2 :scope file
| Headline              | Time   |
|-----------------------+--------|
| *Total time*          | *5:30* |
|-----------------------+--------|
| Write documentation   | 1:15   |
| Fix bugs              | 2:00   |
| Code review           | 2:15   |
#+END:
```

**Generate report**: `C-c C-x C-r` in org file

---

## 🔁 Habit Tracking

**Track recurring tasks** with consistency.

### Create a Habit

```org
* TODO Exercise
  SCHEDULED: <2026-03-15 Sat .+1d>
  :PROPERTIES:
  :STYLE: habit
  :END:
```

**`.+1d`** means "repeat every day starting from completion"

### Habit Graph

In agenda, habits show consistency graph:

```
TODO Exercise        ✓ ✓ ✗ ✓ ✓ ✓ ✗ ✓
```

- ✓ = Done
- ✗ = Skipped
- Shows last 7 days

### Habit Scheduling Options

| Pattern | Meaning | Example |
|---------|---------|---------|
| `.+1d` | Every day from completion | Daily exercise |
| `.+1w` | Every week from completion | Weekly review |
| `+1d` | Every day (strict) | Take medication |
| `++1d` | Every day (skip if late) | Water plants |

---

## 🧠 Org-Roam (Zettelkasten)

**Networked note-taking** for knowledge building.

### Key Concepts

1. **Notes are atomic** - One idea per note
2. **Notes link together** - Build knowledge graph
3. **Tags organize** - Multiple perspectives
4. **Backlinks show connections** - See related notes

### Org-Roam Keybindings

| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC n r f` | Find note | Search/create roam note |
| `SPC n r i` | Insert link | Link to another note |
| `SPC n r g` | Graph | Visualize knowledge graph |
| `SPC n r r` | Roam buffer | Show backlinks |

### Creating a Note

1. `SPC n r f` - Find or create note
2. Type title: "TypeScript Best Practices"
3. Press `Enter` - Creates new note:

```org
#+title: TypeScript Best Practices
#+filetags: :programming:typescript:

* Key Points

** Always use strict mode
...
```

### Linking Notes

```org
In [[id:abc123][TypeScript Best Practices]], we discussed...
                  ↑ Link to other note
```

**Insert link**: `SPC n r i` → Search for note → Inserts link

### Backlinks

**Shows** what other notes reference current note.

```
Backlinks (2):
  - React Patterns
  - Modern JavaScript
```

**Use**: Discover connections, build knowledge web

---

## 🔑 Essential Keybindings

### Capture
| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC X` | Capture | Open capture menu |
| `SPC n n` | Capture (alt) | Alternative binding |
| `C-c C-c` | Finalize | Save and close capture |
| `C-c C-k` | Abort | Cancel capture |

### Navigation
| Keybinding | Command | Description |
|------------|---------|-------------|
| `TAB` | Fold/unfold | Toggle heading visibility |
| `S-TAB` | Fold all | Toggle all headings |
| `C-c C-n` | Next heading | Jump to next heading |
| `C-c C-p` | Previous heading | Jump to previous heading |

### Editing
| Keybinding | Command | Description |
|------------|---------|-------------|
| `M-RET` | New heading | Insert same-level heading |
| `M-left/right` | Promote/demote | Change heading level |
| `M-up/down` | Move heading | Reorder headings |
| `C-c C-t` | TODO state | Cycle TODO/DONE |
| `C-c C-s` | Schedule | Set SCHEDULED date |
| `C-c C-d` | Deadline | Set DEADLINE date |

### Timestamps
| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c .` | Insert date | Pick date with calendar |
| `C-c !` | Insert inactive | Date without agenda |
| `S-left/right` | Adjust date | Change date ±1 day |

---

## 💡 Workflows

### GTD Weekly Review

1. **Open inbox**: `C-x C-f ~/org/inbox.org`
2. **Process each item**:
   - `C-c C-s` - Schedule if time-specific
   - `C-c C-d` - Set deadline if date-critical
   - `C-c C-w` - Refile to projects.org
   - `C-c C-t` - Mark DONE if completed
3. **Review projects**: `C-x C-f ~/org/projects.org`
4. **Update agenda files**: Ensure all active projects tracked

### Daily Planning

1. **Morning**: `SPC X` → `D` (Daily TODO)
2. **Fill template** with today's tasks
3. **Open agenda**: `SPC o a` → `d` (Dashboard)
4. **Start work**: Select task → `I` (clock in)
5. **End of day**: Review completed items

### Zettelkasten Note-Taking

1. **While reading/learning**: `SPC n r f` → Create note
2. **Capture key insights** in note
3. **Link to related notes**: `SPC n r i`
4. **Add tags**: `#+filetags: :concept:programming:`
5. **Over time**: `SPC n r g` to visualize knowledge graph

---

## 🔍 Searching & Filtering

### Search All Org Files

```
M-x org-search-view
```

**Example**: Find all "typescript" references across org files

### Search by Tag

In agenda: `m` → Enter tags: `+work-personal`

**Operators:**
- `+work` - Has tag "work"
- `-personal` - Doesn't have tag "personal"
- `+work-personal` - Has work, not personal
- `work|personal` - Has either tag

### Find TODOs

```
M-x org-todo-list
```

Shows all open TODOs across files.

---

## 🎓 Learning Resources

- [Org-mode Manual](https://orgmode.org/manual/)
- [Org-roam Manual](https://www.orgroam.com/manual.html)
- [GTD with Org-mode](https://orgmode.org/worg/org-gtd.html)
- [Zettelkasten Method](https://zettelkasten.de/introduction/)

---

## 🔗 Related Documentation

- [BUFFER_MANAGEMENT.md](BUFFER_MANAGEMENT.md) - Managing org buffers
- [CLAUDE.md](CLAUDE.md) - Main configuration index

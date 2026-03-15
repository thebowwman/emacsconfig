# TypeScript/React/Next.js Development Configuration

Complete full-stack TypeScript setup for React frontends and Node.js backends (Next.js, Hono, etc.).

---

## 📋 Overview

**LSP Server**: typescript-language-server (tsserver wrapper)
**Formatter**: Prettier
**Linter**: ESLint
**Debugger**: vscode-js-debug (via DAP)
**Node.js**: v20.18.3
**Frameworks**: React, Next.js, Hono

### ✅ Verified Installation

```bash
Node.js:  /usr/bin/node (v20.18.3)
npm:      /usr/bin/npm (v10.8.2)
tsserver: ~/.npm-global/bin/tsserver
ts-lang:  ~/.npm-global/bin/typescript-language-server
Debugger: ~/.config/emacs/.local/cache/.extension/js-debug/src/dapDebugServer.js
```

---

## 🔧 Configuration Details

### LSP (Language Server Protocol)

**Server**: `typescript-language-server` (tsserver wrapper)

**Enhanced Features:**
- ✅ **Auto-imports** - Automatic import suggestions and additions
- ✅ **Organize imports on save** - Removes unused, sorts imports
- ✅ **Auto-update imports on file move** - Renames imports when files move
- ✅ **Type hints** - Inline parameter and return type hints
- ✅ **Code actions** - Quick fixes, refactoring
- ✅ **Relative imports** - Prefers relative over absolute paths
- ✅ **Single quotes** - Consistent quote style

**Configuration:**
```elisp
("typescript.suggest.autoImports" t)
("typescript.updateImportsOnFileMove.enabled" "always")
("typescript.preferences.importModuleSpecifier" "relative")
("typescript.preferences.quoteStyle" "single")
```

### Auto-Organize Imports

Runs automatically on every save for `.ts` and `.tsx` files:

**Before save:**
```typescript
import { useEffect, useState } from 'react';
import axios from 'axios';
import { UserService } from './services/user';  // Unused
import { formatDate } from '../utils/date';
import React from 'react';  // Unused in modern React
```

**After save:**
```typescript
import { useEffect, useState } from 'react';
import axios from 'axios';

import { formatDate } from '../utils/date';
```

**Manual trigger**: `M-x lsp-organize-imports`

### Prettier Formatting

**Auto-formats on save for:**
- TypeScript (`.ts`)
- TSX (`.tsx`)
- JSON (`.json`)
- CSS/SCSS (`.css`, `.scss`)

**What Prettier does:**
- Consistent indentation (2 spaces)
- Semicolons (configurable)
- Single quotes for strings
- Trailing commas
- Line wrapping

**Override per-project:**
Create `.prettierrc` in project root:
```json
{
  "semi": true,
  "singleQuote": true,
  "tabWidth": 2,
  "trailingComma": "es5",
  "printWidth": 80
}
```

### ESLint Integration

**Active linting** for:
- TypeScript syntax errors
- React hooks rules
- Best practices
- Unused variables

**Flycheck shows errors inline** as you type.

**Fix all ESLint errors:**
```
M-x flycheck-buffer
SPC c a  → "Fix all ESLint errors"
```

---

## 🐛 Debugging Setup

### Requirements

✅ **All tools are installed and verified:**

```bash
/usr/bin/node --version
# Output: v20.18.3

/usr/bin/npm --version
# Output: 10.8.2

~/.npm-global/bin/typescript-language-server --version
# Output: Installed ✓

ls ~/.config/emacs/.local/cache/.extension/js-debug/src/dapDebugServer.js
# Output: File exists ✓
```

**ts-node** for TypeScript debugging:
```bash
npm install --save-dev ts-node
```

### TypeScript vs JavaScript Debugging

- **JavaScript (.js)**: Works perfectly with Node debugger
- **TypeScript (.ts)**: Use **ts-node** for reliable source maps
  - Development: Use `tsx` (fast, hot reload)
  - Debugging: Use `ts-node` (better source map support)

### Quick Debug Workflow

**For TypeScript files:**

1. **Open file**: `src/your-file.ts`
2. **Set breakpoint**: `SPC m d b` on desired line (you'll see breakpoint indicator)
3. **Start debugger in terminal**:
   ```bash
   cd ~/Workspace/your-project
   node --require ts-node/register --enable-source-maps --inspect-brk src/your-file.ts
   ```
4. **Attach in Emacs**: `SPC m d d` → **"Node :: Attach (JS/TS)"**
5. **Continue**: Press `SPC m d r` to skip initial break
6. **Debug**: Debugger pauses at your breakpoints!

**For JavaScript files:**

Same workflow, but skip the `--require ts-node/register`:
```bash
node --inspect-brk src/your-file.js
```

### Debug Templates

#### **"Node :: Attach (JS/TS)"**
Attach to running Node/TypeScript process on port 9229.

**Works with:**
- Plain JavaScript
- TypeScript (via ts-node)
- Any Node process started with `--inspect` or `--inspect-brk`

**Keybindings:**
- `SPC m d d` → Select "Node :: Attach (JS/TS)"
- `SPC m d r` → Continue/Resume
- `SPC m d n` → Step to next line
- `SPC m d i` → Step into function
- `SPC m d o` → Step out of function
- `SPC m d b` → Toggle breakpoint
- `SPC m d q` → Quit debugger

#### **"Node :: Launch TS File (ts-node)"**
Launch TypeScript file directly with debugging.

**Usage:**
1. Open TypeScript file
2. `SPC m d d` → "Node :: Launch TS File (ts-node)"
3. Select file when prompted
4. Debugger launches and pauses at breakpoints

**Note:** Breakpoints may not turn green visually, but they WILL pause execution. This is a minor UI issue in dap-mode.

### Debugging Next.js / Hono Servers

**Start server with debugging:**
```bash
# Next.js
NODE_OPTIONS='--inspect' npm run dev

# Hono or custom server
node --require ts-node/register --enable-source-maps --inspect src/index.ts
```

**Then attach:**
1. In Emacs: `SPC m d d` → "Node :: Attach (JS/TS)"
2. Set breakpoints in API routes
3. Make request in browser/curl
4. Debugger pauses at breakpoints!

### Troubleshooting

**Breakpoints not working?**
- Make sure you're using `ts-node` for TypeScript files
- Check that `sourceMap: true` in `tsconfig.json`
- Verify port 9229 is listening: `lsof -i :9229`

**Debugger shows "pending"?**
- Kill existing debug sessions: `pkill -f inspect-brk`
- Restart Emacs to reload debug configuration

**Can't see variables?**
- Press `SPC m d v` to open locals panel
- Hover over variables to see values

---

## 🎨 React & Next.js Snippets

Type abbreviation + `TAB` to expand:

### Basic React Components

#### `rfc` - React Functional Component
```typescript
import React from 'react';

export default function Component() {
  return (
    <div></div>
  );
}
```

#### `rfce` - Component with Fragment
```typescript
import React from 'react';

export default function Component() {
  return (
    <>

    </>
  );
}
```

#### `rfcp` - Component with TypeScript Props
```typescript
import React from 'react';

interface ComponentProps {

}

export default function Component({  }: ComponentProps) {
  return (
    <div></div>
  );
}
```

### React Hooks

#### `useState` - State Hook
```typescript
const [state, setState] = useState();
```

#### `useEffect` - Effect Hook
```typescript
useEffect(() => {

}, []);
```

### Next.js Specific

#### `nextpage` - Next.js Page Component
```typescript
export default function Page() {
  return (
    <div>

    </div>
  );
}
```

#### `nextapi` - Next.js API Route (App Router)
```typescript
import { NextRequest, NextResponse } from 'next/server';

export async function GET(request: NextRequest) {

  return NextResponse.json({ data: null });
}
```

#### `nextlayout` - Next.js Layout
```typescript
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Title',
  description: 'Description',
};

export default function Layout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <>
      {children}
    </>
  );
}
```

### Backend (Hono)

#### `hono` - Hono API Route
```typescript
import { Hono } from 'hono';

const app = new Hono();

app.get('/path', (c) => {
  return c.json({  });
});

export default app;
```

---

## 🔑 TypeScript-Specific Keybindings

### LSP Actions
| Keybinding | Command | Description |
|------------|---------|-------------|
| `g d` | `lsp-find-definition` | Go to definition |
| `g r` | `lsp-find-references` | Find all references |
| `g i` | `lsp-find-implementation` | Go to implementation |
| `K` | `lsp-describe-thing-at-point` | Show type/docs |
| `SPC c r` | `lsp-rename` | Rename symbol across project |
| `SPC c a` | `lsp-execute-code-action` | Code actions (import, fix) |

### Debugging
All DAP keybindings work - see [DAP_DEBUGGING.md](DAP_DEBUGGING.md)

---

## 💡 Workflow Examples

### Example 1: React Component Development

```tsx
// components/UserCard.tsx
// Type 'rfcp' + TAB

import React from 'react';

interface UserCardProps {
  user: User;  // ← Auto-import User type
  onEdit: () => void;
}

export default function UserCard({ user, onEdit }: UserCardProps) {
  // Type 'useState' + TAB
  const [isHovered, setIsHovered] = useState(false);

  // Auto-import works
  const formattedDate = format  // ← Trigger completion
  // Shows: formatDate (from ../utils/date) [Auto-import]

  return (
    <div
      onMouseEnter={() => setIsHovered(true)}  // ← Set breakpoint to debug
      onClick={onEdit}
    >
      {user.name}
    </div>
  );
}
```

**Debug workflow:**
1. Set breakpoint on `onMouseEnter` handler
2. `SPC m d d` → "Next.js :: Debug Server"
3. Hover over card in browser
4. Debugger pauses, inspect `user` and `isHovered`

### Example 2: Next.js API Route

```typescript
// app/api/users/[id]/route.ts
// Type 'nextapi' + TAB

import { NextRequest, NextResponse } from 'next/server';
import { getUserById } from '@/lib/users';  // ← Auto-import

export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  const userId = params.id;

  // Set breakpoint here ← SPC m d b
  const user = await getUserById(userId);

  if (!user) {
    return NextResponse.json(
      { error: 'User not found' },
      { status: 404 }
    );
  }

  return NextResponse.json({ user });
}
```

**Debug workflow:**
1. `SPC m d b` on `getUserById` line
2. `SPC m d d` → "Next.js :: Debug Server"
3. Request in browser: `/api/users/123`
4. Debugger pauses, inspect `userId` and `user`

### Example 3: Hono Backend API

```typescript
// src/routes/users.ts
// Type 'hono' + TAB

import { Hono } from 'hono';
import { z } from 'zod';  // ← Auto-imported

const app = new Hono();

const userSchema = z.object({
  name: z.string(),
  email: z.string().email(),
});

app.post('/users', async (c) => {
  const body = await c.req.json();

  // Set breakpoint ← SPC m d b
  const validated = userSchema.parse(body);

  const user = await db.users.create({
    data: validated
  });

  return c.json({ user }, 201);
});

export default app;
```

**Debug workflow:**
1. `SPC m d b` on `userSchema.parse` line
2. `SPC m d d` → "Node :: Run Current File"
3. POST request via curl/Postman
4. Inspect `body` and `validated` data

### Example 4: File Move Auto-Update

**Before:**
```typescript
// components/UserCard.tsx
import { formatDate } from '../utils/date';
```

**Action:** Move `utils/date.ts` to `lib/formatters/date.ts`

**After (auto-updated):**
```typescript
// components/UserCard.tsx
import { formatDate } from '../../lib/formatters/date';  // ← Auto-updated!
```

---

## 🚀 Performance Tips

### 1. **TypeScript Project References**

For monorepos, use project references:

```json
// tsconfig.json
{
  "references": [
    { "path": "./packages/web" },
    { "path": "./packages/api" }
  ]
}
```

### 2. **Exclude Large Directories**

```json
// tsconfig.json
{
  "exclude": [
    "node_modules",
    ".next",
    "dist",
    "build",
    "coverage"
  ]
}
```

### 3. **Incremental Compilation**

```json
// tsconfig.json
{
  "compilerOptions": {
    "incremental": true
  }
}
```

### 4. **LSP Restart**

If LSP becomes slow:
```
M-x lsp-workspace-restart
```

---

## 🔍 Troubleshooting

### LSP Not Starting

**Check:**
1. Is `typescript-language-server` installed?
   ```bash
   npm list -g typescript-language-server
   ```
2. Is this a TypeScript/JavaScript project? (has `package.json` or `tsconfig.json`)
3. Is Node.js in PATH? `which node`

**Install:**
```bash
npm install -g typescript-language-server typescript
```

**Restart Emacs** or:
```elisp
M-x exec-path-from-shell-initialize
```

### Auto-Imports Not Working

**Verify:**
```elisp
M-x lsp-describe-session
```
Should show TypeScript server active.

**Check configuration:**
```elisp
M-: (lsp-configuration-section "typescript")
```

**Restart LSP:**
```
M-x lsp-workspace-restart
```

### Prettier Not Formatting

**Check:**
1. Is prettier installed?
   ```bash
   npm list prettier
   # or globally
   npm list -g prettier
   ```

2. Is file a supported type? (`.ts`, `.tsx`, `.json`, `.css`)

**Install per-project:**
```bash
npm install --save-dev prettier
```

**Manual format:**
```
M-x prettier-prettify
```

### Imports Not Organizing on Save

**Verify hook:**
```elisp
M-: (member 'lsp-organize-imports before-save-hook)
```
Should return non-nil.

**If false:**
```
SPC h r r   # Reload config
```

### Next.js Debugger Can't Connect

**Issue**: Can't attach to Next.js dev server

**Solution 1: Use debug script**
```json
// package.json
{
  "scripts": {
    "dev": "next dev",
    "dev:debug": "NODE_OPTIONS='--inspect' next dev"
  }
}
```

Run: `npm run dev:debug`, then attach

**Solution 2: Edit debug template**
```elisp
(dap-register-debug-template "Next.js :: Debug Server (Fixed)"
  (list :type "node"
        :request "launch"
        :name "Next.js :: Debug Server (Fixed)"
        :runtimeExecutable "npm"
        :runtimeArgs '("run" "dev:debug")
        :cwd nil
        :console "integratedTerminal"))
```

### Path Aliases Not Resolving

**Issue**: Imports like `@/components/Button` don't autocomplete

**Fix**: Ensure `tsconfig.json` has paths configured:
```json
{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "@/*": ["./*"],
      "@/components/*": ["components/*"],
      "@/lib/*": ["lib/*"]
    }
  }
}
```

**Restart LSP**: `M-x lsp-workspace-restart`

---

## 📦 Recommended Packages

### Development Dependencies
```bash
npm install --save-dev \
  typescript \
  @types/node \
  @types/react \
  @types/react-dom \
  prettier \
  eslint \
  eslint-config-next  # For Next.js
```

### Runtime
```bash
# Next.js
npm install next react react-dom

# Hono
npm install hono

# Utilities
npm install zod          # Validation
npm install date-fns     # Date formatting
```

---

## 🎓 Learning Resources

- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [Next.js Documentation](https://nextjs.org/docs)
- [Hono Documentation](https://hono.dev/)
- [React TypeScript Cheatsheet](https://react-typescript-cheatsheet.netlify.app/)
- [tsserver LSP](https://github.com/typescript-language-server/typescript-language-server)

---

## 🔗 Related Documentation

- [DAP_DEBUGGING.md](DAP_DEBUGGING.md) - Node.js debugging workflow
- [CLAUDE.md](CLAUDE.md) - Main configuration index

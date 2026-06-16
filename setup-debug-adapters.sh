#!/bin/bash
# Setup script for dape debug adapters
# Run this after cloning the doom config on a new machine:
#   chmod +x ~/.doom.d/setup-debug-adapters.sh
#   ~/.doom.d/setup-debug-adapters.sh

set -e

ADAPTER_DIR="${HOME}/.doom.d/debug-adapters"
mkdir -p "$ADAPTER_DIR"

echo "=== Installing debug adapters for dape ==="

# --- js-debug (TypeScript/JavaScript/Chrome) ---
echo ""
echo "[1/4] js-debug (TypeScript/JavaScript/Chrome)..."
if [ -f "$ADAPTER_DIR/js-debug/src/dapDebugServer.js" ]; then
    echo "  Already installed. Skipping."
else
    VERSION=$(curl -s https://api.github.com/repos/microsoft/vscode-js-debug/releases/latest | grep '"tag_name"' | sed 's/.*"tag_name": "\(.*\)".*/\1/')
    if [ -z "$VERSION" ]; then
        echo "  WARNING: Could not fetch latest version (GitHub rate limit?). Using v1.117.0"
        VERSION="v1.117.0"
    fi
    echo "  Downloading js-debug $VERSION..."
    curl -sL "https://github.com/microsoft/vscode-js-debug/releases/download/${VERSION}/js-debug-dap-${VERSION}.tar.gz" \
        -o /tmp/js-debug.tar.gz
    tar -xzf /tmp/js-debug.tar.gz -C "$ADAPTER_DIR/"
    rm /tmp/js-debug.tar.gz
    echo "  Installed to $ADAPTER_DIR/js-debug/"
fi

# --- lldb-dap (Rust/C/C++) ---
echo ""
echo "[2/4] lldb-dap (Rust/C/C++)..."
if command -v lldb-dap-20 &> /dev/null || command -v lldb-dap &> /dev/null; then
    echo "  Already installed: $(which lldb-dap-20 2>/dev/null || which lldb-dap)"
else
    echo "  Not found. Install with:"
    echo "    sudo apt install lldb-20    # Ubuntu/Debian"
    echo "    brew install llvm           # macOS"
fi

# --- delve (Go) ---
echo ""
echo "[3/4] delve (Go)..."
if command -v dlv &> /dev/null; then
    echo "  Already installed: $(which dlv)"
else
    if command -v go &> /dev/null; then
        echo "  Installing..."
        go install github.com/go-delve/delve/cmd/dlv@latest
        echo "  Installed to $(go env GOPATH)/bin/dlv"
    else
        echo "  Go not found. Install Go first, then run:"
        echo "    go install github.com/go-delve/delve/cmd/dlv@latest"
    fi
fi

# --- debugpy (Python) ---
echo ""
echo "[4/4] debugpy (Python)..."
echo "  debugpy is installed per-project in the venv."
echo "  Add to your project: uv add debugpy --dev"
echo "  Or: pip install debugpy"

echo ""
echo "=== Done ==="
echo ""
echo "Restart Emacs and run 'doom sync' to pick up changes."

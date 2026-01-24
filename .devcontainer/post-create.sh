#!/usr/bin/env bash
echo "🔥 Starting post-create.sh..."

set -euo pipefail

echo "🔥 Starting post-create.sh..."

# 1. R packages (step-by-step, no complex chains)
echo "📦 Installing R devtools/pak..."
R -q -e "install.packages(c('devtools', 'usethis', 'pak'), repos='https://cran.rstudio.com/')" || echo "R install continued"

# Only user-level config here (LazyVim)
echo "✨ Setting up LazyVim config for Neovim..."
CONFIG_DIR="$HOME/.config/nvim"
LAZYVIM_REPO="https://github.com/LazyVim/starter"
mkdir -p "$HOME/.config"
if [[ ! -d "$CONFIG_DIR" ]]; then
	git clone "$LAZYVIM_REPO" "$CONFIG_DIR"
	rm -rf "$CONFIG_DIR/.git"
	echo "✅ LazyVim installed"
else
	echo "⚠️ LazyVim already exists - skipping"
fi

echo "✅ ALL SETUP COMPLETE! (System tools are pre-installed via Dockerfile)"

# Ensure ~/.local/bin is in PATH for CLI tools
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
export PATH="$HOME/.local/bin:$PATH"

# Install Claude CLI as rstudio user
echo "🛠️ Installing Claude CLI..."
curl -fsSL https://claude.ai/install.sh | bash

# Install OpenCode CLI as rstudio user
echo "🛠️ Installing OpenCode CLI..."
curl -fsSL https://opencode.ai/install | bash

# Install GitHub Copilot CLI as rstudio user
echo "🛠️ Installing GitHub Copilot CLI..."
curl -fsSL https://gh.io/copilot-install | bash

echo "✅ CLI tools installed and PATH updated."


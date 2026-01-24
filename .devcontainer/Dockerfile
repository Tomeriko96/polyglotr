# Use the official R tidyverse image as the base
FROM rocker/tidyverse:latest

USER root

# Install required tools
RUN apt-get update && \
    apt-get install -y curl vim git && \
    rm -rf /var/lib/apt/lists/*


# Install latest Neovim (system-wide)
RUN set -eux; \
    LATEST_VERSION=$(curl -s https://api.github.com/repos/neovim/neovim/releases/latest | grep '"tag_name"' | sed -E 's/.*"([^"]+)".*/\1/'); \
    NVIM_TARBALL_X64="nvim-linux-x86_64.tar.gz"; \
    NVIM_TARBALL_64="nvim-linux64.tar.gz"; \
    NVIM_URL_X64="https://github.com/neovim/neovim/releases/download/${LATEST_VERSION}/${NVIM_TARBALL_X64}"; \
    NVIM_URL_64="https://github.com/neovim/neovim/releases/download/${LATEST_VERSION}/${NVIM_TARBALL_64}"; \
    cd /tmp; \
    (curl -L --retry 3 --fail -O "$NVIM_URL_X64" || curl -L --retry 3 --fail -O "$NVIM_URL_64"); \
    if [ -f nvim-linux-x86_64.tar.gz ]; then \
        tar -C /usr -xzf nvim-linux-x86_64.tar.gz; \
        ln -sf /usr/nvim-linux-x86_64/bin/nvim /usr/bin/nvim; \
        rm nvim-linux-x86_64.tar.gz; \
    elif [ -f nvim-linux64.tar.gz ]; then \
        tar -C /usr -xzf nvim-linux64.tar.gz; \
        ln -sf /usr/nvim-linux64/bin/nvim /usr/bin/nvim; \
        rm nvim-linux64.tar.gz; \
    fi; \
    nvim --version | head -n1

USER rstudio

#!/usr/bin/env bash
SRC="$HOME/src/dotfiles"

# Shell configuration
link $SRC/sh/zshrc $HOME/.zshrc

# Emacs configuration
link $SRC/emacs/emacs $HOME/.emacs
link $SRC/emacs/emacs.d $HOME/.emacs.d

# Claude configuration
link $SRC/claude/settings.json $HOME/.claude/settings.json
link $SRC/claude/commands $HOME/.claude/commands
link $SRC/claude/scripts $HOME/.claude/scripts

link() {
    local target="$1"
    local src="$2"

    # Create parent directory if it doesn't exist
    mkdir -p "$(dirname "$src")"

    # Remove existing file/link if it exists
    if [ -e "$src" ] || [ -L "$src" ]; then
        echo "Removing $src"
        rm -rf "$src"
    fi

    echo "Linking $src -> $target"
    ln -sf "$target" "$src"
}

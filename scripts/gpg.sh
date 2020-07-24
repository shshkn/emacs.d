#!/usr/bin/env bash

TARGET_DIR="$HOME/.emacs.d/data/gnupg"
[[ -d "$XDG_CONFIG_HOME/emacs" ]] && TARGET_DIR="$XDG_CONFIG_HOME/emacs/data/gnupg"

mkdir -pv "$TARGET_DIR"
chmod 700 "$TARGET_DIR"

if command -v brew; then
  brew install gnupg
fi

CMD=gpg

if command -v gpg2; then
  CMD=gpg2
fi

$CMD --homedir "$TARGET_DIR" --receive-keys 066DAFCB81E42C40

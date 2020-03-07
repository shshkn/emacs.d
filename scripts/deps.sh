#!/usr/bin/env bash

TARGET_DIR="$HOME/.emacs.d/data"
[[ -d "$XDG_CONFIG_HOME/emacs" ]] && TARGET_DIR="$XDG_CONFIG_HOME/emacs/data"

VOIDLINUX=(
  "ripgrep"
  "fd"
  "aspell"
  "aspell-en"
  "aspell-ru"
  "MultiMarkdown"

  # pdf-tools
  "poppler-devel"
  "poppler-glib-devel"

  "sbcl"
  "racket" "libressl-devel"
  "rustup"
  "erlang" "rebar3"
  "elixir"

  "ccls"
  "shellcheck"
)

BREWFILE=(
  "ripgrep"
  "fd"
  "aspell"
  "multimarkdown"

  "poppler"

  "sbcl"
  "minimal-racket"
  "nvm"
  "rustup-init"
  "erlang" "rebar3"
  "elixir"

  "ccls"
  "shellcheck"
  "hadolint"
)

# macOS
if [[ $OSTYPE == darwin* ]]; then
  brew install "${BREWFILE[@]}"
fi

# voidlinux
if command -v xbps-install; then
  sudo xbps-install -Suy "${VOIDLINUX[@]}"

  if command -v brew; then
    brew install nvm
    source "/home/linuxbrew/.linuxbrew/opt/nvm/nvm.sh"
  fi
fi

# pip3 install --user 'python-language-server[all]'
pip3 install --user 'python-language-server[all]==0.31.7'

raco pkg install --auto -u errortrace macro-debugger

if command -v nvm; then

  nvm install node
  npm install -g javascript-typescript-langserver
  npm install -g vscode-html-languageserver-bin
  npm install -g vscode-css-languageserver-bin

  npm install -g elm elm-test elm-format
  npm install -g @elm-tooling/elm-language-server

fi

rustup-init --default-toolchain stable --no-modify-path -y
rustup component add rls rust-analysis rust-src

# Build erlang_ls
ERLANG_LS_SRC=/tmp/erlang_ls
ERLANG_LS_DIR="$TARGET_DIR/erlang_ls"
rm -rf "$ERLANG_LS_SRC" "$ERLANG_LS_DIR"
git clone --depth 1 https://github.com/erlang-ls/erlang_ls "$ERLANG_LS_SRC"
pushd "$ERLANG_LS_SRC" || exit
make
mv "$ERLANG_LS_SRC"/_build/default "$ERLANG_LS_DIR"
rm -rf "$ERLANG_LS_SRC"
popd || exit

# Build elixir-ls
ELIXIR_LS_SRC=/tmp/elixir_ls
ELIXIR_LS_DIR="$TARGET_DIR/elixir_ls"
rm -rf "$ELIXIR_LS_SRC" "$ELIXIR_LS_DIR"
git clone --depth 1 https://github.com/elixir-lsp/elixir-ls "$ELIXIR_LS_SRC"
pushd "$ELIXIR_LS_SRC" || exit
mix deps.get
mix local.hex --force
mix local.rebar --force
mix compile
mix elixir_ls.release -o "$ELIXIR_LS_DIR"
rm -rf "$ELIXIR_LS_SRC"
popd || exit

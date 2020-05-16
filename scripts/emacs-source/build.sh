#!/usr/bin/env bash

EMACS_GIT_URL="git://git.sv.gnu.org/emacs.git"
TARGET_DIR="$HOME/.local/emacs"

SCRIPT_ROOT=$(realpath "$(dirname "$0")")

BRANCH=master

function usage() {
  echo "Usage:"
  echo "    -B specify git branch, default is master"
  echo "    -c configure emacs build"
  echo "    -b build emacs"
  echo "    -i install emacs"
  echo "    -h this help"
  echo "Environment variables:"
  echo "    FEATURES - configure Emacs build, override script's defaults"
  echo '      e.g. FEATURES="--with-ns --disable-ns-self-contained"'
  exit 0
}

[[ $# -eq 0 ]] && usage

while getopts "B:cbih" opt; do
  case "$opt" in
    B)
      BRANCH="$OPTARG"
      ;;
    c)
      CONFIGURE=true
      ;;
    b)
      BUILD=true
      ;;
    i)
      INSTALL=true
      ;;
    h|*)
      usage
      break
      ;;
  esac
done

BRANCH_SUB=${BRANCH//\//_}
SOURCE="$TARGET_DIR/emacs-source-$BRANCH_SUB"
PREFIX="$TARGET_DIR/emacs-bin-$BRANCH_SUB"

INFO=$PREFIX/share/info/emacs
NS_APP=$PREFIX/bin/Emacs.app
NS_APP_BIN=$NS_APP/Contents/MacOS/Emacs

[[ ! -d $SOURCE ]] &&  git clone --depth 100 --branch "$BRANCH" "$EMACS_GIT_URL" "$SOURCE"

cd "$SOURCE" || exit

function configure_build() {
  [[ ! -x "$SOURCE/configure"  ]] && ./autogen.sh

  _args=(
    "-C"
    "--infodir=${INFO}"
    "--prefix=${PREFIX}"
    "--without-all"
    "--without-pop"
    "--with-xml2"
    "--with-json"
    "--with-rsvg"
    "--with-gnutls"
    "--with-threads"
    "--with-modules"
  )

  case $OSTYPE in
    darwin*)
      _args+=("--with-ns"
              "--enable-ns-self-contained"
             )
      ;;
    *)
      _args+=("--with-xpm"
              "--with-jpeg"
              "--with-tiff"
              "--with-gif"
              "--with-png"
              "--with-libotf"
              "--with-x"
              "--with-x-toolkit=no"
              "--with-cairo"
              "--with-harfbuzz"
             )
      ;;
  esac

  read -ra _features <<< "$FEATURES"
  _args+=("${_features[@]}")

  ./configure "${_args[@]}"
}

function build() {
  case $OSTYPE in
    darwin*)
      NCPU=$(sysctl -n hw.logicalcpu)
      ;;
    *)
      NCPU=$(nproc)
      ;;
  esac

  # TEMP:
  _make_flags=()
  [[ $BYTE_COMPILE_EXTRA_FLAGS ]] && _make_flags+=("BYTE_COMPILE_EXTRA_FLAGS=$BYTE_COMPILE_EXTRA_FLAGS")

  make -j"$NCPU" "${_make_flags[@]}"
  make install
}


function install() {
  [[ ! -d "$PREFIX/bin" ]] && [[ ! -d "$SOURCE/nextstep/Emacs.app" ]] \
    && echo "$PREFIX/bin or Emacs.app not found. Build emacs first." && usage

  echo "Installing..."
  case $OSTYPE in
    darwin*)
      mkdir -pv "$PREFIX/bin"
      cp -r "$SOURCE/nextstep/Emacs.app" "$NS_APP" && echo "$NS_APP"

      rm /usr/local/bin/emacs /usr/local/bin/emacsclient
      echo $'#!/bin/bash\nexec' "$NS_APP_BIN" '"$@"' >> /usr/local/bin/emacs &&
        chmod -v +x /usr/local/bin/emacs

      ln -sfv "$PREFIX/bin/emacsclient" /usr/local/bin/emacsclient
      ;;
    *)
      EMACS_BIN="$PREFIX/bin"
      ICON_PATH="$PREFIX/share/icons/hicolor/128x128/apps/emacs.png"
      APPS_PATH="$HOME/.local/share/applications"

      mkdir -pv "$APPS_PATH"

      for f in "$SCRIPT_ROOT"/templates/*.desktop; do
        IN=$f
        OUT=${f##*/}
        sed "s,{PATH},$EMACS_BIN,g; s,{ICON},$ICON_PATH,g" \
            > "$APPS_PATH/$OUT" "$IN"
      done

      rm -v "$HOME/.local/bin/emacs" "$HOME/.local/bin/emacsclient"
      ln -sv "$PREFIX/bin/emacs" "$HOME/.local/bin/emacs"
      ln -sv "$PREFIX/bin/emacsclient" "$HOME/.local/bin/emacsclient"

      mkdir -pv "$HOME/.local/share/icons"
      cp -fv "$ICON_PATH" "$HOME/.local/share/icons/emacs.png"
      ;;
  esac
}

[[ $CONFIGURE ]] && mkdir -pv "$PREFIX" && configure_build
[[ $BUILD ]] && mkdir -pv "$PREFIX" && build
[[ $INSTALL ]] && install

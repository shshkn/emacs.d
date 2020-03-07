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

SOURCE="$TARGET_DIR/emacs-source-$BRANCH"
PREFIX="$TARGET_DIR/emacs-bin-$BRANCH"

INFO=$PREFIX/share/info/emacs
NS_APP=$PREFIX/bin/Emacs.app
NS_APP_BIN=$NS_APP/Contents/MacOS/Emacs

[[ ! -d $SOURCE ]] &&  git clone --depth 1 --branch "$BRANCH" "$EMACS_GIT_URL" "$SOURCE"

cd "$SOURCE" || exit

function configure_build() {
  ./autogen.sh &> /dev/null

  case $OSTYPE in
    darwin*)
      CFLAGS=" $(xml2-config --cflags) $(pkg-config --cflags librsvg-2.0)" \
            ./configure \
            -C \
            --infodir="${INFO}" \
            --prefix="${PREFIX}" \
            --without-all \
            --without-pop \
            --with-xml2 \
            --with-json \
            --with-rsvg \
            --with-gnutls \
            --with-threads \
            --with-ns \
            --disable-ns-self-contained
      ;;
    *)
      ./configure \
        -C \
        --infodir="${INFO}" \
        --prefix="${PREFIX}" \
        --without-all \
        --without-pop \
        --with-xml2 \
        --with-json \
        --with-xpm \
        --with-jpeg \
        --with-tiff \
        --with-gif \
        --with-png \
        --with-rsvg \
        --with-libotf \
        --with-gnutls \
        --with-threads \
        --with-x \
        --with-x-toolkit=no \
        --with-cairo \
        --with-harfbuzz
      ;;
  esac
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

  make -j"$NCPU" install
}


function install() {
  [[ ! -d "$PREFIX/bin" ]] && echo "$PREFIX/bin not found. Build emacs first." && usage

  echo "Installing..."
  case $OSTYPE in
    darwin*)
      [[ -d "$NS_APP" ]] && rm -rv "$NS_APP"

      cp -rv nextstep/Emacs.app "$NS_APP"

      rm -v /usr/local/bin/emacs /usr/local/bin/emacsclient
      echo $'#!/bin/bash\nexec' "$NS_APP_BIN" '"$@"' >> /usr/local/bin/emacs &&
        chmod +x /usr/local/bin/emacs

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

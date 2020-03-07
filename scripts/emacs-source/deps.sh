#!/usr/bin/env bash

case $OSTYPE in
  darwin*)
    brew install \
         pkg-config \
         autoconf \
         gnutls \
         texinfo \
         librsvg \
         jansson
    ;;
  linux-gnu*)
    if command -v xbps-install; then
      sudo xbps-install -Sufy \
           autoconf \
           texinfo \
           gnutls-devel \
           ncurses-devel \
           libXpm-devel \
           libjpeg-turbo-devel \
           libpng-devel \
           giflib-devel \
           tiff-devel \
           librsvg-devel \
           libotf-devel \
           cairo-devel \
           harfbuzz-devel \
           fontconfig \
           freetype \
           libxml2-devel \
           jansson-devel
    fi

    if command -v apt; then
      sudo apt update
      sudo apt install \
           build-essential \
           texinfo \
           libx11-dev \
           libxpm-dev \
           libjpeg-dev \
           libpng-dev \
           libgif-dev \
           libtiff-dev \
           libgtk-3-dev \
           libcairo2-dev \
           libharfbuzz-dev \
           libncurses5-dev \
           libgnutls28-dev
    fi
    ;;
  *)
    exit
    ;;
esac

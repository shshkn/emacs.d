# [gccemacs](http://akrl.sdf.org/gccemacs.html "gccemacs")

## macOS

```
brew install libgccjit

brew --prefix libgccjit
```

## GNU/Linux

[build libgccjit](https://gcc.gnu.org/onlinedocs/jit/internals/index.html#working-on-the-jit-library)

```
GCC_VERSION=$(gcc -dumpversion)
PREFIX="$HOME/.local/gcc-$GCC_VERSION"

git clone --depth 1 --branch releases/gcc-"$GCC_VERSION" git://gcc.gnu.org/git/gcc.git "$PREFIX/src"

cd "$PREFIX/src"

./configure \
    --enable-languages=jit \
    --enable-host-shared \
    --enable-checking=release \
    --disable-bootstrap \
    --disable-multilib \
    --prefix="$PREFIX" \
    --build="$(gcc -dumpmachine)"

make -j"$(nproc)"
make install-strip
```

## Build using [build.sh](../scripts/emacs-source/build.sh) script:

Adjust paths to libgccjit

```
./deps.sh

CFLAGS="-O2 -I/usr/local/opt/libgccjit/include" \
LDFLAGS="-L/usr/local/opt/libgccjit/lib/gcc/10" \
LD_LIBRARY_PATH="/usr/local/opt/libgccjit/lib/gcc/10" \
FEATURES="--with-nativecomp" \
BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 3)"' \
  ./build.sh -B feature/native-comp -c -b -i
```

Use `NATIVE_FULL_AOT=1` for full AOT compilation

You might want to add this to your init.el

``` emacs-lisp
(setq comp-speed 3
      comp-deferred-compilation t)
```

##### image not found
emacs: dlopen(...eln-cache..., 1): image not found
```
cd ./Emacs.app/Contents/
ln -s ./MacOS/libexec/emacs/VER/ARCH/eln-cache eln-cache
```

## Benchmarks (macOS Big Sur)

<https://elpa.gnu.org/packages/elisp-benchmarks.html>

```
./emacs -batch -l ~/.emacs.d/data/packages/elisp-benchmarks-1.8/elisp-benchmarks.el --eval '(progn (setq comp-speed 3) (elisp-benchmarks-run))' -Q
```

#### master (0a5a1adab986de39a147771b8f9aa21656ecc001)

| test               | non-gc avg (s) | gc avg (s) | gcs avg | tot avg (s) | tot avg err (s) |
|--------------------|----------------|------------|---------|-------------|-----------------|
| bubble-no-cons     |           5.38 |       0.05 |       4 |        5.42 |            0.01 |
| bubble             |           2.19 |       5.35 |     485 |        7.54 |            0.02 |
| dhrystone          |           5.58 |       0.00 |       0 |        5.58 |            0.02 |
| fibn-rec           |           4.61 |       0.00 |       0 |        4.61 |            0.12 |
| fibn-tc            |           4.27 |       0.00 |       0 |        4.27 |            0.07 |
| fibn               |           6.32 |       0.00 |       0 |        6.32 |            0.02 |
| flet               |           6.96 |       0.00 |       0 |        6.96 |            0.07 |
| inclist-type-hints |           8.06 |       0.01 |       1 |        8.07 |            0.07 |
| inclist            |           6.72 |       0.01 |       1 |        6.74 |            0.62 |
| listlen-tc         |           3.91 |       0.00 |       0 |        3.91 |            0.01 |
| map-closure        |           6.48 |       0.00 |       0 |        6.48 |            0.06 |
| nbody              |           2.36 |       9.92 |     899 |       12.28 |            0.02 |
| pcase              |           6.32 |       0.00 |       0 |        6.32 |            0.03 |
| pidigits           |           5.96 |       8.58 |     457 |       14.54 |            0.39 |
|--------------------|----------------|------------|---------|-------------|-----------------|
| total              |          75.11 |      23.92 |    1847 |       99.03 |            0.76 |

#### feature/native-comp (323200044f0c3f716f8f78a6f5e39349fe039117)

| test               | non-gc avg (s) | gc avg (s) | gcs avg | tot avg (s) | tot avg err (s) |
|--------------------|----------------|------------|---------|-------------|-----------------|
| bubble-no-cons     |           2.21 |       0.01 |       1 |        2.23 |            0.01 |
| bubble             |           1.42 |       0.14 |       1 |        1.56 |            0.03 |
| dhrystone          |           2.54 |       0.00 |       0 |        2.54 |            0.04 |
| fibn-rec           |           0.00 |       0.00 |       0 |        0.00 |            0.00 |
| fibn-tc            |           0.01 |       0.00 |       0 |        0.01 |            0.00 |
| fibn               |           0.00 |       0.00 |       0 |        0.00 |            0.00 |
| flet               |           1.99 |       0.00 |       0 |        1.99 |            0.01 |
| inclist-type-hints |           0.71 |       0.00 |       0 |        0.71 |            0.02 |
| inclist            |           1.35 |       0.00 |       0 |        1.35 |            0.01 |
| listlen-tc         |           0.14 |       0.00 |       0 |        0.14 |            0.00 |
| map-closure        |           6.35 |       0.00 |       0 |        6.35 |            0.05 |
| nbody              |           1.49 |       0.30 |       1 |        1.78 |            0.02 |
| pcase              |           1.79 |       0.00 |       0 |        1.79 |            0.01 |
| pidigits           |          11.49 |       3.14 |       1 |       14.62 |            0.07 |
|--------------------|----------------|------------|---------|-------------|-----------------|
| total              |          31.48 |       3.59 |       4 |       35.08 |            0.10 |

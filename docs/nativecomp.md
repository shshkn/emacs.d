# [gccemacs](http://akrl.sdf.org/gccemacs.html "gccemacs")

## macOS
[Discussion about building process](https://gist.github.com/mikroskeem/0a5c909c1880408adf732ceba6d3f9ab)

### Compile libgccjit:
[apply patch to gcc formula](https://gist.github.com/mikroskeem/0a5c909c1880408adf732ceba6d3f9ab#1-gcc-with-libgccjit-enabled)
```
HOMEBREW_NO_AUTO_UPDATE=1 brew install gcc --build-from-source --force
```

### Build using [build.sh](../scripts/emacs-source/build.sh) script:

```
./deps.sh

CC="clang" LDFLAGS="-L/usr/local/Cellar/gcc/9.3.0_1/lib/gcc/9" \
FEATURES="--with-nativecomp" \
NATIVE_FAST_BOOT=1 BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 3)"' \
  ./build.sh -B feature/native-comp -c -b
```

You might want to add this to your init.el

``` emacs-lisp
(setq comp-speed 3
      comp-deferred-compilation t)
```

#### image not found
emacs: dlopen(Emacs.app/Contents/MacOS/../lisp/eln-x86_64-apple-darwin19.4.0-95625f7880deb857/custom.eln, 1): image not found
```
cd ./Emacs.app/Contents/
ln -s Resources/lisp lisp
```

## Benchmarks

### <https://elpa.gnu.org/packages/elisp-benchmarks.html>

```
./emacs -batch -l ~/.emacs.d/data/packages/elisp-benchmarks-1.4/elisp-benchmarks.el --eval '(progn (setq comp-speed 3) (elisp-benchmarks-run))' -Q
```

#### master

| test           | non-gc avg (s) | gc avg (s) | gcs avg | tot avg (s) | tot avg err (s) |
|----------------|----------------|------------|---------|-------------|-----------------|
| bubble-no-cons |           7.28 |       0.07 |       4 |        7.35 |            0.00 |
| bubble         |           3.02 |       8.21 |     489 |       11.23 |            0.05 |
| dhrystone      |           6.93 |       0.00 |       0 |        6.93 |            0.01 |
| fibn-rec       |           6.08 |       0.00 |       0 |        6.08 |            0.00 |
| fibn-tc        |           5.53 |       0.00 |       0 |        5.53 |            0.01 |
| fibn           |           7.81 |       0.00 |       0 |        7.81 |            0.02 |
| inclist        |           8.88 |       0.02 |       1 |        8.90 |            0.03 |
| listlen-tc     |           6.17 |       0.00 |       0 |        6.17 |            0.08 |
| nbody          |           2.85 |      13.96 |     839 |       16.81 |            0.17 |
| total          |          54.56 |      22.25 |    1333 |       76.81 |            0.20 |

#### feature/native-comp comp-speed 3

| test           | non-gc avg (s) | gc avg (s) | gcs avg | tot avg (s) | tot avg err (s) |
|----------------|----------------|------------|---------|-------------|-----------------|
| bubble-no-cons |           3.08 |       0.02 |       1 |        3.09 |            0.01 |
| bubble         |           2.04 |       0.24 |       1 |        2.28 |            0.02 |
| dhrystone      |           4.08 |       0.00 |       0 |        4.08 |            0.02 |
| fibn-rec       |           2.30 |       0.00 |       0 |        2.30 |            0.00 |
| fibn-tc        |           1.65 |       0.00 |       0 |        1.65 |            0.01 |
| fibn           |           5.16 |       0.00 |       0 |        5.16 |            0.01 |
| inclist        |           1.74 |       0.02 |       1 |        1.76 |            0.00 |
| listlen-tc     |           0.20 |       0.00 |       0 |        0.20 |            0.00 |
| nbody          |           2.09 |       0.44 |       1 |        2.53 |            0.02 |
| total          |          22.33 |       0.72 |       4 |       23.05 |            0.03 |

### <https://gitlab.com/koral/elisp-benchmarks>

```
git clone --depth 1 https://gitlab.com/koral/elisp-benchmarks /tmp/elisp-benchmarks
./emacs -batch -l /tmp/elisp-benchmarks/elisp-benchmark.el --eval '(progn (setq comp-speed 3) (setq comp-native-path-postfix nil) (elb-run))' -Q
```

```
nbody.el byte time: 153.293672s native time: 26.492198s boost 478.637046%
bubble.el byte time: 31.620088s native time: 6.875261s boost 359.911093%
bubble-no-cons.el byte time: 22.289277s native time: 9.195328s boost 142.397846%
fibn.el byte time: 23.885029s native time: 15.483257s boost 54.263596%
fibn-rec.el byte time: 17.812732s native time: 6.963234s boost 155.811193%
fibn-tc.el byte time: 15.964283s native time: 4.952057s boost 222.376802%
inclist-tc.el byte time: 16.924912s native time: 1.075107s boost 1474.253725%
listlen-tc.el byte time: 15.841278s native time: 0.650274s boost 2336.092786%
inclist-no-type-hints.el byte time: 25.547047s native time: 5.439778s boost 369.634000%
inclist-type-hints.el byte time: 25.829202s native time: 5.428206s boost 375.833121%
dhrystone.el byte time: 62.445176s native time: 36.979301s boost 68.865215%
Total byte time: 133.606350s native time: 39.824325s boost 235.489302%
```

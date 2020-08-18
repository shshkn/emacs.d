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

LDFLAGS="-L/usr/local/Cellar/gcc/10.2.0/lib/gcc/10/" \
FEATURES="--with-nativecomp" \
NATIVE_FAST_BOOT=1 BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq comp-speed 3)"' \
  ./build.sh -B feature/native-comp -c -b -i
```

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

## Benchmarks (macOS Catalina 10.15.4)

### <https://elpa.gnu.org/packages/elisp-benchmarks.html>

```
./emacs -batch -l ~/.emacs.d/data/packages/elisp-benchmarks-1.5/elisp-benchmarks.el --eval '(progn (setq comp-speed 3) (elisp-benchmarks-run))' -Q
```

#### master (5352bda4eeb7415ad2bda5d74e007b4f36021e68)

| test           | non-gc avg (s) | gc avg (s) | gcs avg | tot avg (s) | tot avg err (s) |
|----------------|----------------|------------|---------|-------------|-----------------|
| bubble-no-cons |           6.05 |       0.04 |       4 |        6.09 |            0.27 |
| bubble         |           2.25 |       4.52 |     494 |        6.77 |            0.08 |
| dhrystone      |           6.09 |       0.00 |       0 |        6.09 |            0.03 |
| fibn-rec       |           5.22 |       0.00 |       0 |        5.22 |            0.09 |
| fibn-tc        |           4.96 |       0.00 |       0 |        4.96 |            0.02 |
| fibn           |           6.47 |       0.00 |       0 |        6.47 |            0.08 |
| inclist        |           6.71 |       0.01 |       1 |        6.72 |            0.94 |
| listlen-tc     |           4.25 |       0.00 |       0 |        4.25 |            0.06 |
| nbody          |           2.43 |       7.72 |     839 |       10.15 |            0.14 |
|----------------|----------------|------------|---------|-------------|-----------------|
| total          |          44.42 |      12.29 |    1338 |       56.71 |            1.00 |

#### feature/native-comp (28df049b8d43586d5a91a7b3e1d9e05131572afc) comp-speed 3


| test           | non-gc avg (s) | gc avg (s) | gcs avg | tot avg (s) | tot avg err (s) |
|----------------|----------------|------------|---------|-------------|-----------------|
| bubble-no-cons |           2.28 |       0.01 |       1 |        2.29 |            0.41 |
| bubble         |           1.38 |       0.16 |       1 |        1.54 |            0.03 |
| dhrystone      |           3.53 |       0.00 |       0 |        3.53 |            0.18 |
| fibn-rec       |           1.81 |       0.00 |       0 |        1.81 |            0.43 |
| fibn-tc        |           1.18 |       0.00 |       0 |        1.18 |            0.10 |
| fibn           |           3.31 |       0.00 |       0 |        3.31 |            0.07 |
| inclist        |           1.30 |       0.01 |       1 |        1.31 |            0.04 |
| listlen-tc     |           0.15 |       0.00 |       0 |        0.15 |            0.01 |
| nbody          |           1.50 |       0.30 |       1 |        1.80 |            0.10 |
|----------------|----------------|------------|---------|-------------|-----------------|
| total          |          16.45 |       0.48 |       4 |       16.92 |            0.64 |

### <https://gitlab.com/koral/elisp-benchmarks>

```
git clone --depth 1 https://gitlab.com/koral/elisp-benchmarks /tmp/elisp-benchmarks
./emacs -batch -l /tmp/elisp-benchmarks/elisp-benchmark.el --eval '(progn (setq comp-speed 3) (setq comp-native-path-postfix nil) (elb-run))' -Q
```

```
nbody.el byte time: 93.925446s native time: 17.844818s boost 426.345777%
bubble.el byte time: 19.319558s native time: 4.580786s boost 321.752031%
bubble-no-cons.el byte time: 17.380286s native time: 6.162614s boost 182.027821%
fibn.el byte time: 20.762802s native time: 9.581598s boost 116.694564%
fibn-rec.el byte time: 14.922997s native time: 4.793007s boost 211.349368%
fibn-tc.el byte time: 14.469838s native time: 3.422689s boost 322.762278%
inclist-tc.el byte time: 14.493542s native time: 0.757761s boost 1812.679856%
listlen-tc.el byte time: 14.015246s native time: 0.459715s boost 2948.681466%
inclist-no-type-hints.el byte time: 19.419661s native time: 4.376260s boost 343.750166%
inclist-type-hints.el byte time: 19.140681s native time: 3.829341s boost 399.842688%
dhrystone.el byte time: 52.195308s native time: 31.111822s boost 67.766799%
Total byte time: 97.965531s native time: 28.705556s boost 241.277246%
```

# emacs.d

![preview](./docs/screenshots/preview1.png)

## Installing

Clone the repo into ~/.emacs.d

It's also possible to place configuration in $XDG_CONFIG_HOME/emacs since Emacs version 27.1.

### Building Emacs

``` shell
cd ~/.emacs.d/scripts/emacs-source
./deps.sh
./build.sh -B master -c -b -i
```

### Configuration

Install dependencies (this pulls quite a lot of things)

``` shell
~/.emacs.d/scripts/deps.sh
```

# sh2md

[![Build Status](https://travis-ci.com/kkweon/sh2md.svg?branch=master)](https://travis-ci.com/kkweon/sh2md)
[![Hackage](https://img.shields.io/hackage/v/sh2md.svg?style=flat-square)](http://hackage.haskell.org/package/sh2md)

Record your shell and print in the markdown format

<div align="center">
    <img src="./assets/demo.gif" width="100%">
</div>

```
sh2md --help
```

```
Usage: sh2md [--stdout]
  Record shell and print in markdown

Available options:
  --stdout                 Instead of copying to the clipboard, print the result
                           to stdout
  -h,--help                Show this help text
```


## Build & Install

You need a [stack](https://www.haskellstack.org/)

```
stack build
stack install
```

### nix

```
nix-build # will generate ./result/bin/sh2md
```

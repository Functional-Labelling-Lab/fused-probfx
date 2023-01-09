#!/bin/bash
PREVVERSION=$(ghc --version | sed -rn 's/[^[:digit:]]*([[:digit:]]+.[[:digit:]]+.[[:digit:]]+)/\1/p')
cabal run --with-compiler=ghc-8.4.4

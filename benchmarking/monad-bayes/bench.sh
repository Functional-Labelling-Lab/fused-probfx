#!/bin/bash
PREVVERSION=$(ghc --version | sed -rn 's/[^[:digit:]]*([[:digit:]]+.[[:digit:]]+.[[:digit:]]+)/\1/p')
ghcup set 8.4.4 
cabal run
ghcup set ${PREVVERSION}

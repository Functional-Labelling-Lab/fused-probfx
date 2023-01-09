#!/bin/bash
GREEN="\e[32m"
RED="\e[31m"
RESETCOLOUR="\e[0m"

cd benchmarking
for d in */ ; do
    cd "${d}"
    echo -e "${GREEN}Benchmarking ${d}${RESETCOLOUR}"
    if ./bench.sh ; then
        echo -e "${GREEN}${d} Benchmark Complete${RESETCOLOUR}"
    else
        echo -e "${RED}${d} Benchmark Failed${RESETCOLOUR}"
    fi
    cd ..
done

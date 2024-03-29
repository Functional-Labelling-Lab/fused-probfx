#!/bin/bash
if ! command -v julia &> /dev/null
then
    echo "julia could not be found, please install this (see benchmarking/turing/README.md)"
    exit
fi

# good to go!
cd turing_benchmarks
julia --project=. src/turing_benchmarks.jl

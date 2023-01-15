## Benchmarks
Example programs for fused-probfx.

These can be run through the `examples` target.

```bash
# Options: simLinRegr, lwLinRegr, mhLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, 
# simLogRegr, lwLogRegr, mhLogRegr, simLDA, mhLDA, simRadon, mhRadon and mhSchool.
cabal run examples "output-file.txt" simLDA

# to graph
python3 graph.py "output-file.txt"
```

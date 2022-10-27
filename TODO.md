Weirdness/unsafeness:
- OpenSum.hs line 29/30
- Trace.hs 51
- inference/MH.hs 164, 18, 187


Replace `Effects/State.hs` with `Control.Effect.State`
- already have `Get`, `Put`, `Modify`
- need to understand `handleState`

Replace `Effects/Writer.hs` with `Control.Effect.Writer`
- `write` -> streams
- `tell` writes writes w to program
- `tellM` writes w to model

Replace `Effects/ObsReader.hs` with `Control.Effect.Reader`
- `ask` reads from an observables environment (replace with `ask`)
- need to understand `handleRead`

`Effects/Lift.hs` can use `Control.Effect.Lift`
- Lift monadic comp m into an effect (`Lift m`)
- need to understand `handleLift` (wtf is the apostrophe in `'[Lift m]`)

`Effects/Dist.hs`
- Dist contains distribution, obervables and tags
- Observabless effect?
- Sample takes PrimDistribution and address (runtime int identifier + compile time tag -> string from type)
- wtf is `pattern`?!
- need to understand handleDist

Ideas:
- handlers are equivelent to run functions
- Understand type families and more complex type operators `'`, `~` etc
- State, Read and Write are easiest, need to create a new Observables effect

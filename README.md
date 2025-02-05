# Project for FP course at TU Delft

A clone of `jq` in Haskell. You will find week-by-week description of the task on Brightspace. Full description is ASSIGNMENT.md.

## Build

```bash
cabal build
```

## Test

```bash
cabal test
```

You will need `jq` installed and available on `$PATH` to run `from-upstream` test suite.

## Use

```bash
echo '{"this" : "that"}' | cabal run -- '.this'
```

## Try `jq` online

[play.jqlang.org](https://play.jqlang.org/)

## Docs

[jqlang.org/manual](https://jqlang.org/manual/)

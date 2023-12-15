# linear-dest

Based on lightweight version of [linear-base@e5dbf7dece10134e3b5bbd99c7e82a4bf77b085b](https://github.com/tweag/linear-base/tree/e5dbf7dece10134e3b5bbd99c7e82a4bf77b085b).

At the moment, it cannot be part of the latest version of `linear-base` because of a Cabal bug with Template Haskell in the [custom GHC version supporting destinations](https://github.com/tweag/ghc/tree/tbagrel1/ghc-linear-lets-39509ded-exposed-compact-prims). This will be solved in a near future, and this library is meant to be merged back into `linear-base` at that point.

Template Haskell is used by `linear-generics` and thus by latest versions of `linear-base`.

## Modules about Destination-passing style programming for Haskell

- `src/Compact/Pure.hs`: Public interface of the pure destination API
- `src/Compact/Pure/Internal.hs`: Implementation of the pure destination API
- `bench/Bench`: Benchmark module for destination-based code

## Author

Thomas BAGREL, Tweag.io

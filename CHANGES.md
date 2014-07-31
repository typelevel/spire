## Version 0.8.2

The project now builds for 2.11.2 by default.

Additions:

* added WELL1024a, WELL19937[ac], and WELL44497[ab] RNGs (Dušan Kysel)
* added ziggurat gaussian generator (Dušan Kysel)
* added partial order (Denis Rosset)
* added free algebras (Tom Switzer)
* added optional unicode operators (Erik Osheim)
* added `Trilean`, a three-valued logic type (Erik Osheim)
* added segmented sieve for prime generation (Erik Osheim)
* added Random[A] monad (Erik Osheim)

Fixes:

* specialized group action (Tom Switzer)
* improved WELL512a implementation (Dušan Kysel)
* interval bug fixes (Denis Rosset and Erik Osheim)
* major improvements to fpfilter (Tom Switzer)
* standardized numeric conversions (Erik Osheim)

Changes:

* renamed `BooleanAlgebra[A]` -> `Bool[A]` (Erik Osheim)
* migrated to machinist for ops support (Erik Osheim)
* refactored random package structure (Erik Osheim)

(Versions 0.8.0 and 0.8.1 were skipped due to release problems.)

## Version 0.7.5

*(Need to reconstruct these notes for versions 0.7.5 and earlier.)*

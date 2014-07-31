## Version 0.8.2

The project now builds for 2.11.2 by default.

Additions:

* added `Well1024a`, `Well19937a`, `Well19937c`, `Well44497a`, and `Well44497b` generators (Dušan Kysel)
* added ziggurat gaussian generator (Dušan Kysel)
* added `PartialOrder[A]` (Denis Rosset)
* added free algebras (Tom Switzer)
* added optional unicode operators (Erik Osheim)
* added `Trilean`, a three-valued logic type (Erik Osheim)
* added segmented sieve for prime generation (Erik Osheim)
* added `Random[A]` monad (Erik Osheim)

Fixes:

* specialized `GroupAction` (Tom Switzer)
* improved `Well512a` implementation (Dušan Kysel)
* `Interval[A]` bug fixes (Denis Rosset and Erik Osheim)
* major improvements to `FpFilter` and related machinery (Tom Switzer)
* standardized numeric conversions (Erik Osheim)

Changes:

* renamed `BooleanAlgebra[A]` -> `Bool[A]` (Erik Osheim)
* migrated to [machinist](https://github.com/non/machinist) for ops support (Erik Osheim)
* refactored `spire.random` package structure (Erik Osheim)

(Versions 0.8.0 and 0.8.1 were skipped due to release problems.)

## Version 0.7.5

*(Need to reconstruct these notes for versions 0.7.5 and earlier.)*

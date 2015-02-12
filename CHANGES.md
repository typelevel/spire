## Version 0.9.0

The project now builds with Scala 2.11.4 by default.

This was a major update with many additions, changes, and bug fixes.

Additions:

 * Added and reworked group actions (Denis Rosset)
 * Add Lattices and Heyting algebras (Erik Osheim)
 * More useful symbolic operators (Erik Osheim)
 * Introduce Exponential[A] distribution (Erik Osheim)
 * Introduce NumberTag[A] type class (Erik Osheim)

Fixes:

 * Fix typos (Ben Barnard)
 * Fix possible overflow for Complex norm (William Waites)
 * Reduce macro warnings building Spire (Erik Osheim & Tom Switzer)
 * Add missing Number implicits (Erik Osheim)
 * Replace sys.error with proper Exceptions (Tom Switzer)
 * Fix Rational(0D) constructor bug (Tom Switzer)
 * Fix Trilean bug (Erik Osheim)
 * Fix SafeLong bugs (Erik Osheim)
 * Fix Jet bugs (Robert Garden & Erik Osheim)
 * Fix Interval parsing (Ruediger Klaehn)

Changes:

 * Major interval improvements and bug fixes (Denis Rosset)
 * Improve Gaussian[A] distribution using Ziggurat (Erik Osheim)
 * Use aggreate instead of fold to enable parallelism (Adam Pingel)
 * Clarified and improved partial orders (Denis Rosset)
 * Improve Checked arithmetic macros (Tom Switzer & Erik Osheim)
 * Remove almost all closure allocations in SafeLong (Erik Osheim)
 * Made type classes universal traits where possible
 * Update library dependency versions

## Version 0.8.2

The project now builds for 2.11.2 by default.

Additions:

* added `Well1024a`, `Well19937a`, `Well19937c`, `Well44497a`, and `Well44497b` generators (Dušan Kysel)
* added `Ziggurat` generator for random variables with Gaussian and Exponential distributions (Dušan Kysel)
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

## Version 0.14.0

This release is a major milestone for Spire.

First of all, it introduces a dependency on typelevel/algebra (and
thus on typelevel/cats-kernel). This provides immediate compatibility
with Cats and Algebird.

Second of all, Spire is now published to the org.typelevel
organization (which was previously org.spire-math).

Additions: 
 * Added GCDRing, part of the commutative ring hierarchy
 * Clarified the laws of EuclideanRing
 * Added DivisionRing, a ring in which division is possible
   (generalization of fields to the noncommutative case;
   used now by Spire's quaternions).

Changes:
 * Many type classes are now aliases to algebra's type classes
 * Algebra provides commutative rings and fields, but the
   intemediate structures (GCDRing/EuclideanRing) are added
   by Spire; thus Spire Field differs from algebra Field by
   extending GCDRing and EuclideanRing.
 * The EuclidenRing operations for Field have been corrected;
   in particular, Float, Double no longer perform truncated division
   but a /~ b = 0 for nonzero b, as a can always be divided by b in a
   field.
 * Signed now extends Order, thus IsReal only extends Signed.
 * IsReal has been replaced by Signed for some operations in
   e.g. Complex, making them more precise/general.
 * Many different efficiency improvements

Fixes:
 * Instances for Complex and Quaternion are more precise
 * Numeric[Complex[A]] is no longer provided
 * Many bug fixes

## Version 0.13.0

Additions:
 * Added Eq[Bound[A]] instance
 * Added Interval#overlap and supporting machinery

Changes:
 * Improve Polynomial performance
 * Support negative roots for Real
 * Migrate to newer ScalaCheck

Fixes:
 * Fix bugs in root isolation/refinement for Algebraic
 * Speed up convergence for Rational#limitTo

## Version 0.12.0

Additions:
 * IntervalSeq and IntervalTrie added to spire-extras
 * .toReal and .toAlgebraic methods on Rational

Changes:
 * Package restructuring and improvements
 * Make Interval serializable
 * Deprecate use of SecureJava.fromBytes

Fixes:
 * Numerous bug fixes

## Version 0.11.0

Spire has two new core maintainers: Rüdiger Klaehn and Denis Rosset.

The spire-scalacheck-binding package has been renamed to spire-laws.
The spire-extras package has been introduced, for types that are less
widely-used (but still useful).

Additions:
 * Added spire.math.Merging for merging arrays
 * Added qcombine (for generic Monoids)
 * More benchmarks (Rational, SafeLong, ...)
 * Scalastyle plugin and code clean up
 * Scoverage plugin and coverage tracking
 * Type class instances for java.math.BigInteger
 * Useful .as syntax for converting literals

Changes:
 * Move FixedPoint into extras
 * Improve Rational implemenation (clearer structure, faster)

Fixes:
 * Improved equality (===) support
 * Lots of documentation bug fixes and typo correction
 * Fixed unary - for unsigned int/long
 * Clean up Arbitrary/Gen instances
 * Drastically-improved test coverage for SafeLong, Rational, etc.
 * Fixed bugs around ceil method

## Version 0.10.1

(Version 0.10.0 was aborted due to errors during the release.)

Fixes:

 * Major refactor/improvements to Algebraic (Tom Switzer)
 * Remove unneeded imports (Kenji Yoshida)
 * Fix SBT build (Kenji Yoshida)
 * Update README to display correct mailing list (Pablo Pita)
 * Remove superfluous Interval#min param (Siddhartha Gadgil)
 * Fix code examples in README (Kevin Meredith)
 * Add support for pattern matching SafeLong (Kevin Meredith)
 * Improve Opt and add tests (Alec Zorab)
 * Fix tail recursion with Opt (Kenji Yoshida)
 * Remove ApproximationContext entirely (Erik Osheim)
 * Update sbt, scala and library versions (Kazuhiro Sera)
 * Fix hashCode/equals for polynomials (Erik Osheim)
 * Add pretty stringifcation to polynomials (Rüdiger Klaehn)
 * Many new polynomial tests (Rüdiger Klaehn)
 * Fix cooperative equality on Rational (Erik Osheim)
 * Create DESIGN.md for implementation docs (Rüdiger Klaehn)
 * Improve Interval tests, fix bugs (Denis Rosset)
 * Add PCG-based Generator (Alexey Romanov)
 * Fix Float/Double GCD bugs (Tom Switzer)
 * Improve SafeLong test coverage (Erik Osheim)
 * Improve Rational test coverage, fix bugs (Rüdiger Klaehn)
 * Many Rational efficiency improvements (Rüdiger Klaehn)
 * Add scoverage support (Rüdiger Klaehn)
 * Fix ULong bugs (Erik Osheim)

## Version 0.9.1

 * Interval bug fixes (Denis Rosset)
 * Efficiency improvements for Rational (Rüdiger Klaehn)
 * Improve and document NumberTag (Tom Switzer)
 * Add support and tests for partial actions (Denis Rosset)
 * Add Opt[_] type for unboxed optional values (Denis Rosset)
 * Add Gitter chat room to README (Erik Osheim)
 * Fix bug with root-finding for Long values (Erik Osheim)

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

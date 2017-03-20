## Design for conversions/homomorphisms

Our goal: a generic conversion type class `Conversion[A, B]` without falling into the `ambiguous implicit` trap

One possible solution: define manually nStart x nEnd instances of conversions

We try the following rules to obtain a solution with less boilerplate that still clarifies implicit derivation.

Implicit instances for fixed A and B are OK, i.e. `implicit object Conv extends Conversion[A, B] { ... }`, as they supercede any implicit derivation made through an `implicit def`. However, they can be made available by default (i.e. through companion object and not an explicit import) in those cases:

- in a user library, conversions from a user type to Spire type, or from a Spire type to user type
- in a user library, conversions from a user type to Scala stdlib type, or from a Scala stdlib type to user type

### Automatic instance derivation

Source types `A` cannot define a conversion to a generic `B`; however, they can make themselves available for conversion through definition typeclasses (such as `IsInZ`, `IsInQ`, `IsInR`) which are then consumed by the destination type.

Destination types `B` can implement conversion in those cases:

- when lifting `A` into `F[A]`. Example: `Rational` to `Complex[Rational]`

- when lifting `A`, which can be converted to `B`, into `F[B]`. Example: `Int` to `Complex[SafeLong]`

- they can implement conversions of the type `F[C] => F[D]` (`F` is the destination type that owns these conversions). Example: `Complex[SafeLong]` to `Complex[Rational]`.

- they can implement conversions of the type `F[C] => G[D]` (`G` is the destination type that owns these conversions). Example: `Complex[SafeLong]` to `Quaternion[Rational]`.

Currently, the primitive integer types `Byte`, `Short`, `Int`, `Long` are covered as sources by `IsInZ`, and as destinations by `IsZ`. These are treated as genuine integers, which happen to have a runtime size limitation, and whose behavior is undefined when the size limitation is triggered.

The big integer types `SafeLong`, `BigInt`, `BigInteger` are also covered by `IsInZ` and `IsZ`.

`Rational` is made available through `IsInQ` (all integers are available through `IsInQ` as well).

Types can make themselves available as `Double` approximations through `IsInR` (however, their `Rational` and `BigDecimal` approximations require a parameter controlling the precision).

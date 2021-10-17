package spire
package syntax

trait FastForSyntax:
  import macros._
  import collection.immutable.NumericRange

  final type RangeLike = Range | NumericRange[Long]

  final type RangeElem[X <: RangeLike] = X match
    case Range              => Int
    case NumericRange[Long] => Long

  inline def fastFor[A](inline init: A)(inline test: A => Boolean, inline next: A => A)(inline body: A => Unit): Unit =
    fastForInline(init, test, next, body)

  inline def fastForRange[R <: RangeLike](inline r: R)(inline body: RangeElem[R] => Unit): Unit =
    ${ fastForRangeMacroGen('r, 'body) }

  inline def fastForRange2[R <: RangeLike](inline r1: R, inline r2: R)(inline body: (RangeElem[R], RangeElem[R]) => Unit): Unit =
    fastForRange(r1) { x => fastForRange(r2) { y => body(x, y) } }
end FastForSyntax
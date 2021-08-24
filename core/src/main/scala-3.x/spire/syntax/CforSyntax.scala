package spire
package syntax
//
// import spire.NoImplicit
// import spire.algebra._
// import spire.algebra.lattice._
// import spire.algebra.partial._
// import spire.math._
// // import spire.macros.Syntax
// import spire.syntax.std._
// import scala.annotation.nowarn
//
// trait CforSyntax {
//   def cfor[A](init: A)(test: A => Boolean, next: A => A)(body: A => Unit): Unit =
//     var index = init
//     while (test(index)) {
//       body(index)
//       index = next(index)
//     }
//   def cforRange(r: Range)(body: Int => Unit): Unit =
//     ???
//     // macro Syntax.cforRangeMacro
//   // def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit =
//   //   macro Syntax.cforRange2Macro
// }

trait CforSyntax:
  import macros._
  import collection.immutable.NumericRange

  final type RangeLike = Range | NumericRange[Long]

  final type RangeElem[X <: RangeLike] = X match
    case Range              => Int
    case NumericRange[Long] => Long

  inline def cfor[A](inline init: A)(inline test: A => Boolean, inline next: A => A)(inline body: A => Unit): Unit =
    cforInline(init, test, next, body)

  inline def cforRange[R <: RangeLike](inline r: R)(inline body: RangeElem[R] => Unit): Unit =
    ${ cforRangeMacroGen('r, 'body) }

  inline def cforRange2[R <: RangeLike](inline r1: R, inline r2: R)(inline body: (RangeElem[R], RangeElem[R]) => Unit): Unit =
    cforRange(r1) { x => cforRange(r2) { y => body(x, y) } }

  /** Alias of [[cforRange]] as an infix method.
   */
  // inline def [R <: RangeLike](inline r: R) peek(inline body: RangeElem[R] => Unit): Unit =
  //   cforRange(r)(body)

// object cfor extends CforSyntax

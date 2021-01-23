package spire
package syntax

import spire.NoImplicit
import spire.algebra._
import spire.algebra.lattice._
import spire.algebra.partial._
import spire.math._
// import spire.macros.Syntax
import spire.syntax.std._
import scala.annotation.nowarn

trait CforSyntax {
  def cfor[A](init: A)(test: A => Boolean, next: A => A)(body: A => Unit): Unit =
    var index = init
    while (test(index)) {
      body(index)
      index = next(index)
    }
  def cforRange(r: Range)(body: Int => Unit): Unit =
    ???
    // macro Syntax.cforRangeMacro
  // def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit =
  //   macro Syntax.cforRange2Macro
}

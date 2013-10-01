package spire.math.poly

import compat._
import scala.reflect._
import scala.{specialized => spec}
import spire.math._
import spire.algebra._
import spire.syntax._


// Lexicographic ordering
// e.g. x^2 < xy < xz < x < y^2 < yz < y < z^2 < z < 1
trait MonomialOrderingLex[@spec(Float, Double) C] extends Order[Monomial[C]] {

  def compare(x: Monomial[C], y: Monomial[C]): Int =
    (x.exps.isEmpty, y.exps.isEmpty) match {
      case (true, true) => 0
      case (false, true) => -1
      case (true, false) => 1
      case (false, false) => x.firstNonZeroVarIndex() compare y.firstNonZeroVarIndex() match {
        case -1 => -1
        case 1 => 1
        case 0 => x.firstNonZeroVarExp(x.exps) compare y.firstNonZeroVarExp(y.exps) match {
          case -1 => 1
          case 1 => -1
          case 0 => compare(x.monomialTail, y.monomialTail)
        }
      }
    }

}

// Graded lexicograpic ordering
// e.g. x^2 < xy < xz < y^2 < yz < z^2 < x < y < z < 1
trait MonomialOrderingGlex[@spec(Float, Double) C] extends Order[Monomial[C]] {

  def compare(x: Monomial[C], y: Monomial[C]): Int = ???

}

// Graded reverse lexicographic ordering
// e.g. x^2 < xy < y^2 < xz < yz < z^2 < x < y < z < 1
trait MonomialOrderingGrevlex[@spec(Float, Double) C] extends Order[Monomial[C]] {

  def compare(x: Monomial[C], y: Monomial[C]): Int = ???

}


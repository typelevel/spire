package spire.math.poly

import compat._
import spire.math._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}


// Lexicographic ordering
// e.g. x^2 < xy < xz < x < y^2 < yz < y < z^2 < z < 1
trait MonomialOrderingLex[@spec(Float, Double) C] extends Order[Monomial[C]] {

  def compare(x: Monomial[C], y: Monomial[C]): Int = ???

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


package spire.math.poly

import compat._
import spire.math._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}

object SpecialPolynomials {

  def hornerScheme[C](zero: Polynomial[C], one: Polynomial[C], 
                      fn: (Polynomial[C], Polynomial[C], Int) => Polynomial[C])
                     (implicit r: Ring[C], s: Signed[C]): Stream[Polynomial[C]] = {
    def loop(pnm1: Polynomial[C], pn: Polynomial[C], n: Int = 1): Stream[Polynomial[C]] = {
      pn #:: loop(pn, fn(pn, pnm1, n), n + 1)
    }
    zero #:: loop(zero, one)
  }

  // Legendre generator function - sparse and accurate Rational type
  private[this] def legendreFnRational: (Polynomial[Rational], Polynomial[Rational], Int) => Polynomial[Rational] = 
    (pn: Polynomial[Rational], pnm1: Polynomial[Rational], n: Int) => {
      val a = Polynomial(Rational(1, n + 1), 0)
      val b = Polynomial(Rational(2 * n + 1), 1)
      val c = Polynomial(-Rational(n), 0)
      a * (b * pn + c * pnm1)
    }

  // Legendre generator function - dense Double type for fast arithmetic
  private[this] def legendreFnDouble: (Polynomial[Double], Polynomial[Double], Int) => Polynomial[Double] = 
    (pn: Polynomial[Double], pnm1: Polynomial[Double], n: Int) => {
      val a = Polynomial(1.0 / (n + 1.0), 0).toDense
      val b = Polynomial(2.0 * n + 1, 1).toDense
      val c = Polynomial(-n.toDouble, 0).toDense
      a * (b * pn + c * pnm1)
    }

  val legOneRational: Polynomial[Rational] = Polynomial(Rational(1), 0)
  val legXRational: Polynomial[Rational] = Polynomial(Rational(1), 1)
  val legOneDouble: Polynomial[Double] = Polynomial.dense(Array(1.0))
  val legXDouble: Polynomial[Double] = Polynomial.dense(Array(0.0, 1.0))

  def legendresRational(num: Int): Stream[Polynomial[Rational]] = 
    hornerScheme(legOneRational, legXRational, legendreFnRational).take(num)

  def legendresDouble(num: Int): Stream[Polynomial[Double]] = 
    hornerScheme(legOneDouble, legXDouble, legendreFnDouble).take(num)

}
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

  // Horner scheme polynomial generator stream
  def hornerScheme[C: Ring: Signed: ClassTag](zero: Polynomial[C], one: Polynomial[C], 
                      fn: (Polynomial[C], Polynomial[C], Int) => Polynomial[C]): Stream[Polynomial[C]] = {
    def loop(pnm1: Polynomial[C], pn: Polynomial[C], n: Int = 1): Stream[Polynomial[C]] = {
      pn #:: loop(pn, fn(pn, pnm1, n), n + 1)
    }
    zero #:: loop(zero, one)
  }

  // Legendre recurrence function
  private[this] def legendreFn[C: Signed: ClassTag](implicit f: Field[C]): (Polynomial[C], Polynomial[C], Int) => Polynomial[C] = 
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => {
      val a = Polynomial(Map(0 -> f.fromInt(1) / f.fromInt(n + 1)))
      val b = Polynomial(Map(1 -> f.fromInt(2 * n + 1)))
      val c = Polynomial(Map(0 -> -f.fromInt(n)))
      a * (b * pn + c * pnm1)
    }

  // Chebyshev recurrence function
  private[this] def chebyshevFn[C: Signed: ClassTag](implicit r: Ring[C]): (Polynomial[C], Polynomial[C], Int) => Polynomial[C] =
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => {
      Polynomial.twox[C] * pn - pnm1
    }

  // Hermite recurrence function for probability
  private[this] def hermiteFnProb[C: Signed: ClassTag](implicit r: Ring[C]): (Polynomial[C], Polynomial[C], Int) => Polynomial[C] =
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => {
      Polynomial.x[C] * pn - pn.derivative
    }

  // Hermite recurrence function for physics
  private[this] def hermiteFnPhys[C: Signed: ClassTag](implicit r: Ring[C]): (Polynomial[C], Polynomial[C], Int) => Polynomial[C] =
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => {
      Polynomial.twox[C] * pn - pn.derivative
    }

  // Legendre polynomials of the first kind
  def legendres[C: Field: Signed: ClassTag](num: Int): Stream[Polynomial[C]] = 
    hornerScheme(Polynomial.one[C], Polynomial.x[C], legendreFn[C]).take(num)

  // Chebyshev polynomials of the first kind
  def chebyshevsFirstKind[C: Ring: Signed: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.x[C], chebyshevFn[C]).take(num)

  // Chebyshev polynomials of the second kind
  def chebyshevsSecondKind[C: Ring: Signed: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.twox[C], chebyshevFn[C]).take(num)

  // Probability hermite polynomials
  def probHermites[C: Ring: Signed: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.x[C], hermiteFnProb[C]).take(num)

  // Physics hermite polynomials
  def physHermites[C: Ring: Signed: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.twox[C], hermiteFnPhys[C]).take(num)

}
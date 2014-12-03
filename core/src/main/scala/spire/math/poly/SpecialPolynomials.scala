package spire.math.poly

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => spec}

import spire.algebra.{Eq, Field, Ring}
import spire.math.Polynomial
import spire.syntax.field._

object SpecialPolynomials {

  // Horner scheme polynomial generator stream
  def hornerScheme[C: Ring: Eq: ClassTag](zero: Polynomial[C], one: Polynomial[C], 
                   fn: (Polynomial[C], Polynomial[C], Int) => Polynomial[C]): Stream[Polynomial[C]] = {
    def loop(pnm1: Polynomial[C], pn: Polynomial[C], n: Int = 1): Stream[Polynomial[C]] = {
      pn #:: loop(pn, fn(pn, pnm1, n), n + 1)
    }
    zero #:: loop(zero, one)
  }

  // Legendre recurrence function
  private[this] def legendreFn[C: Eq: ClassTag](implicit f: Field[C]): (Polynomial[C], Polynomial[C], Int) => Polynomial[C] = 
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => {
      val a = Polynomial(Map(0 -> f.fromInt(1) / f.fromInt(n + 1)))
      val b = Polynomial(Map(1 -> f.fromInt(2 * n + 1)))
      val c = Polynomial(Map(0 -> -f.fromInt(n)))
      a * (b * pn + c * pnm1)
    }

  // Laguerre recurrence function
  private[this] def laguerreFn[C: Eq: ClassTag](implicit f: Field[C]): (Polynomial[C], Polynomial[C], Int) => Polynomial[C] = 
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => {
      Polynomial(Map(0 -> f.one / f.fromInt(n + 1))) * (Polynomial(Map(0 -> f.fromInt(2 * n + 1), 1 -> -f.one)) * pn - pnm1 * Polynomial(Map(0 -> f.fromInt(n))))
    }

  // Chebyshev recurrence function
  private[this] def chebyshevFn[C: Ring: Eq: ClassTag]: (Polynomial[C], Polynomial[C], Int) => Polynomial[C] =
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => Polynomial.twox[C] * pn - pnm1

  // Hermite recurrence function for probability
  private[this] def hermiteFnProb[C: Ring: Eq: ClassTag]: (Polynomial[C], Polynomial[C], Int) => Polynomial[C] =
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => Polynomial.x[C] * pn - pn.derivative

  // Hermite recurrence function for physics
  private[this] def hermiteFnPhys[C: Ring: Eq: ClassTag]: (Polynomial[C], Polynomial[C], Int) => Polynomial[C] =
    (pn: Polynomial[C], pnm1: Polynomial[C], n: Int) => Polynomial.twox[C] * pn - pn.derivative

  // Legendre polynomials of the first kind
  def legendres[C: Field: Eq: ClassTag](num: Int): Stream[Polynomial[C]] = 
    hornerScheme(Polynomial.one[C], Polynomial.x[C], legendreFn[C]).take(num)

  // Laguerre polynomials
  def laguerres[C: Eq: ClassTag](num: Int)(implicit f: Field[C]): Stream[Polynomial[C]] = 
    hornerScheme(Polynomial.one[C], Polynomial(Map(0 -> f.one, 1 -> -f.one)), laguerreFn[C]).take(num)

  // Chebyshev polynomials of the first kind
  def chebyshevsFirstKind[C: Ring: Eq: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.x[C], chebyshevFn[C]).take(num)

  // Chebyshev polynomials of the second kind
  def chebyshevsSecondKind[C: Ring: Eq: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.twox[C], chebyshevFn[C]).take(num)

  // Probability hermite polynomials
  def probHermites[C: Ring: Eq: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.x[C], hermiteFnProb[C]).take(num)

  // Physics hermite polynomials
  def physHermites[C: Ring: Eq: ClassTag](num: Int): Stream[Polynomial[C]] =
    hornerScheme(Polynomial.one[C], Polynomial.twox[C], hermiteFnPhys[C]).take(num)
}

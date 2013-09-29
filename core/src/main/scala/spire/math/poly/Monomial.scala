package spire.math.poly

import compat._
import scala.reflect._
import scala.{specialized => spec}
import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax._


// A monomial is the product of a coefficient and a list of variables each to a non-negative integer power.
case class Monomial[@spec(Float, Double) C: ClassTag](coeff: C, exps: Array[Int]) { lhs =>

  def eval(vars: Array[C])(implicit r: Semiring[C]): C = {
    var prod = coeff
    cfor(0)(_ < exps.length , _ + 1) { i => prod *= (vars(i) ** exps(i)) }
    prod
  }

  def unary_-(implicit r: Rng[C]): Monomial[C] = Monomial(-coeff, exps)

  def +(rhs: Monomial[C])(implicit r: Semiring[C], eq: Eq[C]): Monomial[C] = {
    if (lhs.exps === rhs.exps)
      throw new IllegalArgumentException(s"can't add monomials with different number of variables or different exponents!")
    Monomial(lhs.coeff + rhs.coeff, lhs.exps)
  }

  def *(rhs: Monomial[C])(implicit r: Semiring[C]): Monomial[C] = {
    if (lhs.exps.length != rhs.exps.length)
      throw new IllegalArgumentException(s"can't add monomials with different number of variables")
    Monomial(lhs.coeff * rhs.coeff, lhs.exps + rhs.exps)
  }

  def isZero(implicit r: Semiring[C], eq: Eq[C]): Boolean =
    coeff === r.zero

  def divideBy(x: C)(implicit f: Field[C]): Monomial[C] =
    Monomial(coeff / c, lhs.exps)

}

  override def toString = {
    import Monomial._

    // TODO: Figure out how to print these monomials!

  }

}

object Monomial {

  def apply[@spec(Float, Double) C](coeff: C, exps: Array[Int])
                                   (implicit r: Rig[C]): Monomial[C] =
    Monomial(coeff, exps)

  def zero[@spec(Float, Double) C](implicit r: Rig[C]): Monomial[C] =
    Monomial(r.zero, Array(0))
  
  def one[@spec(Float, Double) C](implicit r: Rig[C]): Monomial[C] = 
    Monomial(r.one, Array(1))

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r
}


trait MonomialEq[@spec(Float, Double) C] extends Eq[Monomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def eqv(x: Monomial[C], y: Monomial[C]): Boolean =
    x.exps === y.exps
}



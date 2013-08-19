package spire.math.poly

import compat._
import spire.math._
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}

// Univariate polynomial term
case class Term[@spec(Float, Double) C: ClassTag](coeff: C, exp: Int)(implicit r: Ring[C], s: Signed[C]) { lhs =>

  def unary_-(): Term[C] = Term(-coeff, exp)

  def +(rhs: Term[C]): Term[C] = {
    if (lhs.exp != rhs.exp)
      throw new IllegalArgumentException(s"can't add terms of degree $exp and $e")
    Term(lhs.coeff + rhs.coeff, lhs.exp)
  }

  def *(rhs: Term[C]): Term[C] =
    Term(lhs.coeff * rhs.coeff, lhs.exp + rhs.exp)

  def toTuple: (Int, C) = (exp, coeff)

  def eval(x: C): C = 
    coeff * (x pow exp)

  def isIndexZero: Boolean = 
    exp == 0

  def isZero: Boolean =
    coeff.signum == 0

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der: Term[C] =
    Term(coeff * r.fromInt(exp), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt(exp + 1), exp + 1)

  override def toString = {
    val pos = coeff.signum >= 0
    (coeff, exp) match {
      case (0, _) => ""
      case (c, 0) => if (pos) s" + ${c}" else s" - ${-c}"
      case (1, e) => if (e > 1) s" + x^$e" else s" + x"
      case (-1, e) => if (e > 1) s" - x^$e" else s" - x"
      case (c, 1) => if (pos) s" + ${c}x" else s" - ${-c}x"
      case (c, e) => if (pos) s" + ${c}x^$e" else s" - ${-c}x^$e"
    }
  }
}

object Term {
  implicit def ordering[C] = new Order[Term[C]] {
    def compare(x: Term[C], y: Term[C]): Int = x.exp compare y.exp
  }

  def fromTuple[@spec(Float, Double) C: ClassTag](tpl: (Int, C))(implicit r: Ring[C], s: Signed[C]): Term[C] = 
    Term(tpl._2, tpl._1)
  def zero[@spec(Float, Double) C: ClassTag](implicit r: Ring[C], s: Signed[C]): Term[C] = 
    Term(r.zero, 0)
  def one[@spec(Float, Double) C: ClassTag](implicit r: Ring[C], s: Signed[C]): Term[C] = 
    Term(r.one, 0)

}

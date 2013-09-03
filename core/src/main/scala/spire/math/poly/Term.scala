package spire.math.poly

import compat._
import spire.math._
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}

// Univariate polynomial term
case class Term[@spec(Float, Double) C](coeff: C, exp: Int) { lhs =>

  def unary_-(implicit r: Rng[C]): Term[C] = Term(-coeff, exp)

  def +(rhs: Term[C])(implicit r: Semiring[C]): Term[C] = {
    if (lhs.exp != rhs.exp)
      throw new IllegalArgumentException(s"can't add terms of degree $exp and $e")
    Term(lhs.coeff + rhs.coeff, lhs.exp)
  }

  def *(rhs: Term[C])(implicit r: Semiring[C]): Term[C] =
    Term(lhs.coeff * rhs.coeff, lhs.exp + rhs.exp)

  def toTuple: (Int, C) = (exp, coeff)

  def eval(x: C)(implicit r: Semiring[C]): C =
    if (exp != 0) coeff * (x pow exp) else coeff

  def isIndexZero: Boolean = 
    exp == 0

  def isZero(implicit s: Signed[C]): Boolean =
    coeff.isZero

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der(implicit r: Ring[C]): Term[C] =
    Term(coeff * r.fromInt(exp), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt(exp + 1), exp + 1)

  override def toString = {
    import Term._

    def expString = exp match {
      case 0 => ""
      case 1 => "x"
      case _ => s"x^$exp"
    }

    def simpleCoeff: Option[String] = coeff match {
      case 0 => Some("")
      case 1 if exp == 0 => Some(s" + $coeff")
      case 1 => Some(s" + $expString")
      case -1 if exp != 0 => Some(s" - $expString")
      case _ => None
    }

    def stringCoeff: Option[String] = coeff.toString match {
      case IsZero() => Some("")
      case IsNegative(posPart) if exp == 0 => Some(s" - $posPart")
      case IsNegative(posPart) => Some(s" - $posPart$expString")
      case _ => None
    }

    simpleCoeff orElse stringCoeff getOrElse s" + $coeff$expString"
  }
}

object Term {
  implicit def ordering[C] = new Order[Term[C]] {
    def compare(x: Term[C], y: Term[C]): Int = x.exp compare y.exp
  }

  def fromTuple[@spec(Float, Double) C: ClassTag](tpl: (Int, C)): Term[C] = 
    Term(tpl._2, tpl._1)
  def zero[@spec(Float, Double) C: ClassTag](implicit r: Rng[C]): Term[C] =
    Term(r.zero, 0)
  def one[@spec(Float, Double) C: ClassTag](implicit r: Rig[C]): Term[C] = 
    Term(r.one, 0)

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r
}

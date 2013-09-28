package spire.math.poly

import compat._
import spire.math._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}

// A monomial is a list of terms (the head containing the coefficient, the tail containing the variables - all with coefficients == one)
case class Monomial[@spec(Float, Double) C](variables: List[Term[C]]) { lhs =>

  // should have a check of length match here...
  def eval(vars: List[C])(implicit r: Ring[C]): C = 
    (variables.head.eval(vars.head) :: variables.tail.zip(vars.tail).map({ 
      case (t, v) => t.eval(v) })).qproduct

  def unary_-(implicit r: Rng[C]): Monomial[C] = Monomial(-variables.head :: variables.tail)

  def +(rhs: Monomial[C])(implicit r: Semiring[C]): Monomial[C] = {
    if (lhs.variables.length != rhs.variables.length && lhs.variables.zip(rhs.variables).forall(z => z._1.exp == z._2.exp))
      throw new IllegalArgumentException(s"can't add monomials with different number of variables or different exponents!")
    Monomial(lhs.variables.head + rhs.variables.head :: lhs.variables.tail)
  }

  def *(rhs: Monomial[C])(implicit r: Semiring[C]): Monomial[C] = {
    if (lhs.variables.length != rhs.variables.length)
      throw new IllegalArgumentException(s"can't add monomials with different number of variables")
    Monomial(lhs.variables.zip(rhs.variables).map(z => z._1 * z._2))
  }

  def isIndexZero: Boolean = 
    variables.forall(_.isIndexZero)

  def isZero(implicit ring: Semiring[C], eq: Eq[C]): Boolean =
    variables.head.isZero

  def divideBy(x: C)(implicit f: Field[C]): Monomial[C] =
    Monomial(variables.head.divideBy(x) :: variables.tail)

  // TODO: Derivatives with respect to different variables... e.g. d/dx, d/dy or d/dz (is this really necessary though for mvpolys?)

  override def toString = {
    import Monomial._

    // def expString(variableLetter: String, exp: Int) = exp match {
    //   case 0 => ""
    //   case 1 => s"$variableLetter"
    //   case _ => s"$variableLetter^$exp"
    // }

    variables.mkString("") // outputting the Terms (but each one is basically just a different variable..)
    // e.g. x y z...
  }

}

object Monomial {
  implicit def ordering[C] = new Order[Monomial[C]] {
    def compare(x: Monomial[C], y: Monomial[C]): Int = x.variables.head.exp compare y.variables.head.exp
  }

  def apply[@spec(Float, Double) C](coeff: C, exps: List[Int])
                                   (implicit r: Rig[C]): Monomial[C] =
    Monomial(Term(coeff, exps.head) :: exps.tail.map(e => Term(r.one, e)))

  def zero[@spec(Float, Double) C](implicit r: Rig[C]): Monomial[C] =
    Monomial(r.zero, List(0))
  
  def one[@spec(Float, Double) C](implicit r: Rig[C]): Monomial[C] = 
    Monomial(r.one, List(0))

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r
}

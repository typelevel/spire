package spire.math.poly

import compat._
import scala.annotation.tailrec
import scala.reflect._
import scala.{specialized => spec}
import spire.algebra._
import spire.implicits._
import spire.math._


// A monomial is the product of a coefficient and a list of variables (Char as symbol) 
// each to a non-negative integer power.
case class Monomial[@spec(Double) C](coeff: C, vars: Map[Char, Int])
                                    (implicit val ct: ClassTag[(Char, Int)]) { lhs =>

  lazy val degree: Int = vars.values.sum

  lazy val variables: Array[(Char, Int)] = {
    val arr = vars.toArray
    QuickSort.sort(arr)(Order[(Char, Int)], implicitly[ClassTag[(Char, Int)]])
    arr
  }

  def isZero(implicit r: Semiring[C], eq: Eq[C]): Boolean =
    coeff === r.zero

  def unary_-(implicit r: Rng[C]): Monomial[C] = 
    Monomial(-coeff, vars)

  def divideBy(x: C)(implicit f: Field[C]): Monomial[C] =
    Monomial(coeff / x, lhs.vars)

  def multiplyBy(x: C)(implicit r: Ring[C]): Monomial[C] =
    Monomial(coeff * x, lhs.vars)

  def divides(rhs: Monomial[C])(implicit o: Order[C]): Boolean =
    lhs.coeff <= rhs.coeff && lhs.variables.zip(rhs.variables).forall({ case(a,b) => a <= b })

  def gcd(rhs: Monomial[C])(implicit f: Field[C], er: EuclideanRing[C]): Monomial[C] = 
    Monomial(er.gcd(lhs.coeff, rhs.coeff), lhs.vars.zip(rhs.vars).map({case (a,b) => min(a,b)}))

  def lcm(rhs: Monomial[C])(implicit f: Field[C], er: EuclideanRing[C]): Monomial[C] = 
    Monomial(er.lcm(lhs.coeff, rhs.coeff), lhs.vars.zip(rhs.vars).map({case (a,b) => max(a,b)}))

  override def toString = {
    import Monomial._

    val varStr = variables.map(v => v._2 match {
      case 0 => ""
      case 1 => s"${v._1}"
      case e => s"${v._1}^$e"
    }).mkString

    def simpleCoeff: Option[String] = coeff match {
      case 0 => Some("")
      case 1 => Some(s" + $varStr")
      case -1 if variables(0)._2 != 0 => Some(s" - $varStr")
      case _ => None
    }

    def stringCoeff: Option[String] = coeff.toString match {
      case IsZero() => Some("")
      case IsNegative(posPart) => Some(s" - $posPart$varStr")
      case _ => None
    }

    simpleCoeff orElse stringCoeff getOrElse s" + $coeff$varStr"

  }
}

object Monomial {

  def apply[@spec(Double) C: ClassTag](c: C, v: (Char, Int)*): Monomial[C] =
    Monomial(c, v.filterNot(_._2 == 0).toMap)

  def zero[@spec(Double) C: ClassTag](implicit r: Rig[C]): Monomial[C] =
    Monomial(r.zero, Map[Char, Int]())
  
  def one[@spec(Double) C: ClassTag](implicit r: Rig[C]): Monomial[C] = 
    Monomial(r.one, Map('x' -> 1))

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r

}


package spire.math.poly

import scala.collection.SortedMap
import scala.annotation.tailrec
import scala.reflect._
import scala.{specialized => spec}
import spire.algebra._
import spire.implicits._
import spire.math._
import scala.util.matching.Regex


/**
 *  A monomial is the product of a coefficient and a list of
 *  variables (Char as symbol) each to an integer power.
*/
class Monomial[@spec(Double) C: ClassTag: Order] private[spire] (val coeff: C, val vars: SortedMap[Char, Int])
  (implicit r: Ring[C]) { lhs =>

  def degree: Int = vars.values.sum

  def isZero: Boolean = lhs == Monomial.zero[C]

  def isConstant: Boolean = vars.isEmpty

  def eval(values: Map[Char, C]): C = {
    val m = evalPartial(values)
    if (m.isConstant) m.coeff
    else throw new Exception("Must pass in values for all variables when evaluating.")
  }

  def evalPartial(values: Map[Char, C]): Monomial[C] = {
    val newCoeff = coeff * vars.map {
      case (v, exp) => values.getOrElse(v, r.one) ** exp
    }.reduce(_ * _)
    val newVars = vars.filterNot {
      case (k,v) => values contains k
    }.toList
    Monomial[C](newCoeff, newVars)
  }

  def unary_- = Monomial[C](-coeff, lhs.vars)

  def *:(x: C): Monomial[C] = Monomial[C](coeff * x, lhs.vars)

  def :*(k: C): Monomial[C] = k *: lhs

  def :/(x: C)(implicit f: Field[C]): Monomial[C] =
    lhs.*:(x.reciprocal)

  def *(rhs: Monomial[C])(implicit eqm: Eq[Monomial[C]]): Monomial[C] =
    if(lhs.isZero || rhs.isZero) Monomial.zero[C]
    else Monomial[C](lhs.coeff * rhs.coeff, lhs.vars + rhs.vars)

  // n.b. only monomials with the same variables form a ring or field
  // but it is like this as we do the arithmetic in MultivariatePolynomial.
  def add(rhs: Monomial[C]): Monomial[C] = {
    if (rhs.isZero) lhs
    else if (lhs.isZero) rhs
    else if (lhs.vars == rhs.vars) Monomial[C](lhs.coeff + rhs.coeff, lhs.vars)
    else throw new NumberFormatException("Cannot add unlike terms; use Multivariate Polynomial instead")
  }

  def subtract(rhs: Monomial[C]): Monomial[C] = lhs add (-rhs)

  def /(rhs: Monomial[C])(implicit f: Field[C]): Monomial[C] =
    if(lhs == rhs) Monomial.one[C]
    else Monomial[C](lhs.coeff / rhs.coeff, (lhs.vars - rhs.vars).toList)

  def divides(rhs: Monomial[C])(implicit ordChar: Order[Char], ordInt: Order[Int]): Boolean = {
    if(lhs.degree == 0 && rhs.degree == 0 && lhs.coeff.abs < rhs.coeff.abs) true else if(lhs.degree == 0 && rhs.degree == 0) false else
      lhs.vars.view.zip(rhs.vars).forall(z => (z._1._2 <= z._2._2))
  }

  def gcd(rhs: Monomial[C])(implicit er: EuclideanRing[C]): Monomial[C] = {
    @tailrec def gcd_(z: SortedMap[Char, Int], x: SortedMap[Char, Int], y: SortedMap[Char, Int]) : Monomial[C] = {
      if(x.isEmpty || y.isEmpty)
        Monomial[C](er.gcd(lhs.coeff, rhs.coeff), z)
      else x.head._1 compare y.head._1 match {
        case -1 => gcd_(z, x.tail, y)
        case 1 => gcd_(z, x, y.tail)
        case 0 => {
          val k: Int = min(x.head._2, y.head._2)
          gcd_(SortedMap(x.head._1 -> k) ++ z, x.tail, y.tail)
        }
      }
    }
    gcd_(SortedMap[Char, Int](), lhs.vars, rhs.vars)
  }

  def lcm(rhs: Monomial[C])(implicit er: EuclideanRing[C]): Monomial[C] = {
    @tailrec def lcm_(z: SortedMap[Char, Int], x: SortedMap[Char, Int], y: SortedMap[Char, Int]) : Monomial[C] = {
      if(x.isEmpty || y.isEmpty)
        Monomial[C](er.lcm(lhs.coeff, rhs.coeff), z)
      else x.head._1 compare y.head._1 match {
        case -1 => lcm_(z, x.tail, y)
        case 1 => lcm_(z, x, y.tail)
        case 0 => {
          val k: Int = max(x.head._2, y.head._2)
          lcm_(SortedMap(x.head._1 -> k) ++ z, x.tail, y.tail)
        }
      }
    }
    lcm_(SortedMap[Char, Int](), lhs.vars, rhs.vars)
  }

  def toPoly: MultivariatePolynomial[C] = MultivariatePolynomial[C](this)

  override def equals(that: Any): Boolean = that match {
    case rhs: Monomial[C] if lhs.coeff === rhs.coeff && lhs.vars == rhs.vars => true
    case _ => false
  }

  override def toString = {

    // import Monomial._

    val varStr = vars.foldLeft("") {
      case (str, (v, 0)) => str
      case (str, (v, 1)) => str + v
      case (str, (v, exp)) => s"$str$v^$exp"
    }

    val coeffStr =
      if (!isConstant && coeff.isOne) ""
      else if (!isConstant && coeff == -r.one) "-"
      else coeff

    coeffStr + varStr
  }

}

object Monomial {

  def apply[@spec(Double) C: ClassTag: Order: Ring](c: C, v: (Char, Int)*): Monomial[C] =
    checkCreateMonomial(c, v.toArray)

  def apply[@spec(Double) C: ClassTag: Order: Ring](c: C, v: List[(Char, Int)]): Monomial[C] =
    checkCreateMonomial(c, v.toArray)

  def apply[@spec(Double) C: ClassTag: Order: Ring](c: C, v: Map[Char, Int]): Monomial[C] =
    checkCreateMonomial(c, v.toArray)

  def apply[@spec(Double) C: ClassTag: Order: Ring](c: C, v: SortedMap[Char, Int]): Monomial[C] =
    checkCreateMonomial(c, v.toArray)

  def apply(str: String): Monomial[Rational] = parseFractional[Rational](str)

  def apply[@spec(Double) C: ClassTag: Order: Ring](c: Char): Monomial[C] =
    variable(c)

  def apply[@spec(Double) C: ClassTag: Order: Ring](c: C): Monomial[C] =
    constant(c)

  def checkCreateMonomial[@spec(Double) C: ClassTag](c: C, arr: Array[(Char, Int)])
    (implicit r: Ring[C], o: Order[C]): Monomial[C] = c match {
      case n if r.isZero(n) => zero[C]
      case _ => {
        arr.length match {
          case 0 => constant(c)
          case _ => {
            val map = arr.foldLeft(SortedMap[Char,Int]()) {
              // combine any like vars so x^2x^3 becomes x^5
              case (m, (v, exp)) =>
                val initialExp = m.getOrElse(v, 0)
                m + ((v, exp + initialExp))
            }.filterNot {
              // remove any variables to the 0 power
              case (v, exp) => exp == 0
            }
            new Monomial[C](c, map)
          }
        }
      }
    }

  def zero[@spec(Double) C: ClassTag: Order](implicit r: Ring[C]): Monomial[C] =
    new Monomial[C](r.zero, SortedMap[Char, Int]())

  def one[@spec(Double) C: ClassTag: Order](implicit r: Ring[C]): Monomial[C] =
    new Monomial[C](r.one, SortedMap[Char, Int]())

  def constant[@spec(Double) C: ClassTag: Order](c: C)(implicit r: Ring[C]): Monomial[C] =
    new Monomial[C](c, SortedMap[Char, Int]())

  def variable[@spec(Double) C: ClassTag: Order](v: Char)(implicit r: Ring[C]): Monomial[C] =
    new Monomial[C](r.one, SortedMap(v -> 1))

  def x[@spec(Double) C: ClassTag: Order](implicit r: Ring[C]): Monomial[C] =
    new Monomial[C](r.one, SortedMap('x' -> 1))

  object Re {
    val variable = "[\u03B1-\u03C9a-zA-Z]".r
    val numFractional = "[0-9]+(?:[/.][0-9]+)?".r
    val numIntegral = "[0-9]+".r
    val varExp = s"($variable)(?:\\^(-?[0-9]+))?".r

    def term(number: Regex): Regex = s"(- *)?($number)?($varExp)*".r
    val termFractional = term(numFractional)
    val termIntegral = term(numIntegral)
  }


    // val terms = s"($negative)? *($number)?($varchar(\\^(-)?$number)?)*".r

  final def parseFractional[@spec(Double) C: ClassTag: Fractional](str: String): Monomial[C] = {

    val f = implicitly[Field[C]]
    val c = implicitly[ConvertableTo[C]]

    val stringToC: String => C = (str: String) => str.split('/') match {
      case (Array(numerator, denominator)) =>
        c.fromDouble(numerator.toDouble) / c.fromDouble(denominator.toDouble)
      case (Array(number)) => c.fromDouble(number.toDouble)
      case _ => throw new Exception("Could not parse number " + str)
    }

    parseGeneric(str, Re.termFractional, stringToC, f.one)
  }

  final def parseIntegral[@spec(Int, Long) C: Integral: ClassTag](str: String): Monomial[C] = {

    val r = implicitly[Integral[C]]
    val c = implicitly[ConvertableTo[C]]

    val stringToC: String => C = (str: String) => c.fromBigInt(BigInt(str))

    parseGeneric(str, Re.termIntegral, stringToC, r.one)
  }

  private [poly] final def parseGeneric[@spec(Double, Int, Long) C: ClassTag: Order: ConvertableTo: Ring](str: String, terms: Regex, stringToC: String => C, one: C): Monomial[C] = {

    val m: Option[Monomial[C]] = terms.findFirstMatchIn(str).map { term =>
      val coeff: C = term.subgroups.map(Option(_)) match {
        case (List(Some(neg), Some(coeff), _, _, _)) => -stringToC(coeff)
        case (List(None, Some(coeff), _, _, _)) => stringToC(coeff)
        case (List(Some(neg), None, _, _, _)) => -one
        case (List(None, None, _, _, _)) => one
        case _ => one
      }
      val exponents = Re.varExp.findAllMatchIn(term.matched).toList.map {
        _.subgroups match {
          case (List(variable, null)) => variable(0) -> 1
          case (List(variable, exp)) => variable(0) -> exp.toInt
          case _ => throw new Exception("Couldn't parse term " + term)
        }
      }
      Monomial[C](coeff, exponents)
    }
    m getOrElse(throw new Exception("Could not parse term "+str))
  }

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r

  implicit def monomialEq[@spec(Double) C: ClassTag: Order: Semiring] = new MonomialEq[C] {
    val scalar = Semiring[C]
    val ct = implicitly[ClassTag[C]]
  }

}

// An equivalent monomial has the same variables (that's all!)
// not checking that the variable exponents are equal using this instance
trait MonomialEq[@spec(Double) C] extends Eq[Monomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def ct: ClassTag[C]
  def eqv(x: Monomial[C], y: Monomial[C]): Boolean =
    x.vars.toArray === y.vars.toArray
}

// Lexicographic ordering
// e.g. x^2 > xy > xz > x > y^2 > yz > y > z^2 > z > 1
trait MonomialOrderingLex[@spec(Double) C] extends Order[Monomial[C]]
with MonomialEq[C] {

  implicit def ordCoeff: Order[C]
  implicit val ordChar = Order[Char]
  implicit val ordInt = Order[Int]

  override def eqv(x: Monomial[C], y: Monomial[C]): Boolean =
    x.vars.toArray === y.vars.toArray

  def compare(l: Monomial[C], r: Monomial[C]): Int = {
    @tailrec def compare_(x: SortedMap[Char, Int], y: SortedMap[Char, Int]): Int = {
      (x.isEmpty, y.isEmpty) match {
        case (true, true) => l.coeff compare r.coeff
        case (false, true) => -1
        case (true, false) => 1
        case _ => ordChar.compare(x.head._1, y.head._1) match {
          case -1 => -1
          case 1 => 1
          case 0 => ordInt.compare(x.head._2, y.head._2) match {
            case -1 => 1
            case 1 => -1
            case 0 => compare_(x.tail, y.tail)
          }
        }
      }
    }
    compare_(l.vars, r.vars)
  }

}

// Graded lexicographic ordering
// e.g. x^2 > xy > xz > y^2 > yz > z^2 > x > y > z > 1
trait MonomialOrderingGlex[@spec(Double) C] extends Order[Monomial[C]]
with MonomialEq[C] {

  implicit def ordCoeff: Order[C]
  implicit val ordChar = Order[Char]
  implicit val ordInt = Order[Int]

  override def eqv(x: Monomial[C], y: Monomial[C]): Boolean =
    x.vars.toArray === y.vars.toArray

  def compare(l: Monomial[C], r: Monomial[C]): Int = {
    @tailrec def compare_(x: SortedMap[Char, Int], y: SortedMap[Char, Int]): Int = {
     (x.isEmpty, y.isEmpty) match {
        case (true, true) => l.coeff compare r.coeff
        case (false, true) => -1
        case (true, false) => 1
        case _ => ordInt.compare(x.values.sum, y.values.sum) match {
          case -1 => 1
          case 1 => -1
          case 0 => ordChar.compare(x.head._1, y.head._1) match {
            case -1 => -1
            case 1 => 1
            case 0 => ordInt.compare(x.head._2, y.head._2) match {
              case -1 => 1
              case 1 => -1
              case 0 => compare_(x.tail, y.tail)
            }
          }
        }
      }
    }
    compare_(l.vars, r.vars)
  }
}

//Graded reverse lexicographic ordering
// e.g. x^2 > xy > y^2 > xz > yz > z^2 > x > y > z
trait MonomialOrderingGrevlex[@spec(Double) C] extends Order[Monomial[C]]
with MonomialEq[C] {

  implicit def ordCoeff: Order[C]
  implicit val ordChar = Order[Char]
  implicit val ordInt = Order[Int]

  override def eqv(x: Monomial[C], y: Monomial[C]): Boolean =
    x.vars.toArray === y.vars.toArray

  def compare(l: Monomial[C], r: Monomial[C]): Int = {
    @tailrec def compare_(x: SortedMap[Char, Int], y: SortedMap[Char, Int]): Int = {
      (x.isEmpty, y.isEmpty) match {
        case (true, true) => l.coeff compare r.coeff
        case (false, true) => -1
        case (true, false) => 1
        case _ => ordInt.compare(x.values.sum, y.values.sum) match {
          case -1 => 1
          case 1 => -1
          case 0 => ordChar.compare(x.head._1, y.head._1) match {
            case -1 => -1
            case 1 => 1
            case 0 => compare_(x.tail, y.tail)
          }
        }
      }
    }
    compare_(SortedMap(l.vars.toArray.reverse:_*), SortedMap(r.vars.toArray.reverse:_*))
  }
}

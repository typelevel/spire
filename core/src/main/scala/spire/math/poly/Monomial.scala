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

  def divides(rhs: Monomial[C]): Boolean = {
    @tailrec def divides_(x: Map[Char, Int], y: Map[Char, Int]): Boolean = {
      if(x.isEmpty) true else if(y.isEmpty) false else x.head._1 compare y.head._1 match {
        case -1 => false
        case 1 => divides_(x, y.tail)
        case 0 => if(x.head._2 <= y.head._2) divides_(x.tail, y.tail) else false
      }
    }
    divides_(lhs.vars, rhs.vars)
  }

  def gcd(rhs: Monomial[C])(implicit er: EuclideanRing[C]): Monomial[C] = {
    @tailrec def gcd_(z: Map[Char, Int], x: Map[Char, Int], y: Map[Char, Int]) : Monomial[C] = {
      if(x.isEmpty || y.isEmpty) Monomial(er.gcd(lhs.coeff, rhs.coeff), z) else x.head._1 compare y.head._1 match {
        case -1 => gcd_(z, x.tail, y)
        case 1 => gcd_(z, x, y.tail)
        case 0 => {
          val k: Int = min(x.head._2, y.head._2)
          gcd_(Map(x.head._1 -> k) ++ z, x.tail, y.tail)
        }
      } 
    }
    gcd_(Map[Char, Int](), lhs.vars, rhs.vars)
  }

  def lcm(rhs: Monomial[C])(implicit er: EuclideanRing[C]): Monomial[C] = {
    @tailrec def lcm_(z: Map[Char, Int], x: Map[Char, Int], y: Map[Char, Int]) : Monomial[C] = {
      if(x.isEmpty || y.isEmpty) Monomial(er.lcm(lhs.coeff, rhs.coeff), z) else x.head._1 compare y.head._1 match {
        case -1 => lcm_(z, x.tail, y)
        case 1 => lcm_(z, x, y.tail)
        case 0 => {
          val k: Int = max(x.head._2, y.head._2)
          lcm_(Map(x.head._1 -> k) ++ z, x.tail, y.tail)
        }
      } 
    }
    lcm_(Map[Char, Int](), lhs.vars, rhs.vars)
  }

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

  // Must get rid of zero order variables at instantiation and sort the variables in the Map!
  def apply[@spec(Double) C: ClassTag](c: C, v: (Char, Int)*): Monomial[C] = {
    val arr = v.filterNot(_._2 == 0).toArray
    QuickSort.sort(arr)(Order[(Char, Int)], implicitly[ClassTag[(Char, Int)]])
    Monomial(c, arr.toMap) 
  }

  def zero[@spec(Double) C: ClassTag](implicit r: Rig[C]): Monomial[C] =
    Monomial(r.zero, Map[Char, Int]())
  
  def one[@spec(Double) C: ClassTag](implicit r: Rig[C]): Monomial[C] = 
    Monomial(r.one, Map('x' -> 1))

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r

  // implicit def lexOrdering[C] = new MonomialOrderingLex[C] {}
  implicit def glexOrdering[C] = new MonomialOrderingGlex[C] {}

}


trait MonomialOrderingLex[@spec(Double) C] extends Order[Monomial[C]] {
  def compare(x: Monomial[C], y: Monomial[C]): Int = {
    @tailrec def compare_(x: Map[Char, Int], y: Map[Char, Int]): Int = {
      if(x.isEmpty && y.isEmpty) 0 else if(y.isEmpty) -1 else if(x.isEmpty) 1 else {
        x.head._1 compare y.head._1 match {
          case -1 => -1
          case 1 => 1
          case 0 => x.head._2 compare y.head._2 match {
            case -1 => 1
            case 1 => -1
            case 0 => compare_(x.tail, y.tail)
          }
        }
      }
    }
    compare_(x.vars, y.vars)
  }
}

trait MonomialOrderingGlex[@spec(Double) C] extends Order[Monomial[C]] {
  def compare(x: Monomial[C], y: Monomial[C]): Int = {
    @tailrec def compare_(x: Map[Char, Int], y: Map[Char, Int]): Int = {
      if(x.isEmpty && y.isEmpty) 0 else if(y.isEmpty) -1 else if(x.isEmpty) 1 else {
        x.values.sum compare y.values.sum match {
          case -1 => 1
          case 1 => -1
          case 0 => x.head._1 compare y.head._1 match {
              case -1 => -1
              case 1 => 1
              case 0 => x.head._2 compare y.head._2 match {
                case -1 => 1
                case 1 => -1
                case 0 => compare_(x.tail, y.tail)
              }
            }
        }  
      }
    }
    compare_(x.vars, y.vars)
  }
}

trait MonomialOrderingGrevlex[@spec(Double) C] extends Order[Monomial[C]] {
  // TODO: implement graded reverse lexographic ordering. Should be easy.
}

/* Array(Monomial(1.0, 'x' -> 2),
Monomial(1.0, 'x' -> 1, 'y' -> 1),
Monomial(1.0, 'x' -> 1, 'z' -> 1),
Monomial(1.0, 'x' -> 1),
Monomial(1.0, 'y' -> 2),
Monomial(1.0, 'y' -> 1, 'z' -> 1),
Monomial(1.0, 'y' -> 1),
Monomial(1.0, 'z' -> 2),
Monomial(1.0, 'z' -> 1))
*/

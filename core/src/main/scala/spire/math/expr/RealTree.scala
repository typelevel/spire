package spire.math
package expr

import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

object RealTree extends TrigTree {
  case class RationalNode(r: Rational) extends AtomNode
  case class FloatNode(bd: BigDecimal, powerOfTen: Int) extends AtomNode

  val oneNode = RationalNode(Rational.one)
  val zeroNode = RationalNode(Rational.zero)
  def nodeFromInt(n: Int) = RationalNode(Rational(n))

  override def toCompact(displayNode: Node): Node = displayNode match {
    case c: ConstantNode => c
    case r: RationalNode => r
    case f: FloatNode => f
    case _ => super.toCompact(displayNode)
  }

  override def toDisplay(compactNode: Node): Node = compactNode match {
    case c: ConstantNode => c
    case RationalNode(r) if r < 0 => 
      NegNode(toDisplay(RationalNode(-r)))
    case RationalNode(r) if r.denominator != 1 =>
      DivNode(RationalNode(r.numerator), RationalNode(r.denominator))
    case r: RationalNode => r
    case FloatNode(bd, powerOfTen) if bd < 0 =>
      NegNode(FloatNode(-bd, powerOfTen))
    case f: FloatNode => f
    case _ => super.toDisplay(compactNode)
  }

  protected def exactRationalValue(node: Node): Rational = node match {
    case _: FloatNode => sys.error("Cannot transform a FloatNode to Rational.")
    case RationalNode(r) => r
    case PlusNode(seq) => seq.map(exactRationalValue(_)).reduce(_+_)
    case TimesNode(seq) => seq.map(exactRationalValue(_)).reduce(_*_)
    case NegNode(n) => -exactRationalValue(n)
    case InvNode(n) => exactRationalValue(n).reciprocal
    case _ => sys.error("Cannot transform node to Rational.")
  }

  override def trySimplified(compactNode: Node): Option[Node] = {
    compactNode match {
      case NegNode(RationalNode(r)) => return Some(RationalNode(-r))
      case InvNode(RationalNode(r)) => return Some(RationalNode(r.reciprocal))
      case NegNode(FloatNode(bd, powerOfTen)) => return Some(FloatNode(-bd, powerOfTen))
      case PlusNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
        val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
        val r = exactRationalValue(PlusNode(rational))
        if (r != 0)
          return Some(PlusNode(nonRational :+ RationalNode(r)))
        else
          return Some(PlusNode(nonRational))
      case TimesNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
        val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
        val r = exactRationalValue(TimesNode(rational))
        if (r == 0)
          return Some(RationalNode(r))
        if (r != 1)
          return Some(TimesNode(nonRational :+ RationalNode(r)))
        else
          return Some(TimesNode(nonRational))
      case _ =>
    }
    super.trySimplified(compactNode)
  }

  object TreeParser extends TrigTreeParserTrait {
    lazy val atom: PackratParser[Node] = float | integer | constant

    lazy val constant: PackratParser[Node] = 
      ("pi" ^^^ ConstantNode("pi")) |
      ("e" ^^^ ConstantNode("e"))

    lazy val float: PackratParser[Node] =
      (bigdecimal ~ opt(("e" | "E") ~> int))  ^^ { 
        case bd ~ None => FloatNode(bd, 0)
        case bd ~ Some(exp) => FloatNode(bd, exp)
      }

    lazy val integer: PackratParser[Node] =
      biginteger ^^ { case i => RationalNode(Rational(i)) }

    val int = """-?(0|([1-9]\d*))""".r ^^ { case s: String => s.toInt }
    val biginteger = """0|([1-9]\d*)""".r ^^ { case s: String => BigInt(s) }
    val bigdecimal = """(0|([1-9]\d*))?[.]\d*""".r ^^ { case s: String => BigDecimal(s) }
  }

  object TreeUnparser extends TrigTreeUnparserTrait {
    // force printing the BigDecimal with a dot
    def printBigDecimal(bd: BigDecimal): String = bd.scale match {
      case 0 => bd.toString + "."
      case _ => bd.toString
    }

    override def printable(n: Node): Printable = n match {
      case ConstantNode(name) => Value(name)
      case RationalNode(r) =>
        require(r >= 0)
        require(r.denominator == 1)
        Value(r.toString)
      case FloatNode(bd, 0) => 
        require(bd >= 0)
        Value(printBigDecimal(bd))
      case FloatNode(bd, powerOfTen) =>
        require(bd >= 0)
        Value(s"${printBigDecimal(bd)}e${powerOfTen}")
      case _ => super.printable(n)
    }
  }
}

object RealTreeEvaluator extends TrigEvaluator[RealTree.type, Real] {
  val tree = RealTree
  implicit val scalarAlgebra = Real.algebra
  def fpow(a: Real, b: Real) = a.fpow(b)
  override def value(node: tree.Node): Real = node match {
    case tree.RationalNode(r) => Real(r)
    case tree.FloatNode(bd, powerOfTen) => Real(bd) * Real(10).fpow(powerOfTen)
    case _ => super.value(node)
  }
}

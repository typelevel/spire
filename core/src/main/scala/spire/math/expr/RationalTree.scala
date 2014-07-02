package spire.math
package expr

import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

trait RationalTree extends FieldTree {
  case class RationalNode(r: Rational) extends AtomNode

  val oneNode = RationalNode(Rational.one)
  val zeroNode = RationalNode(Rational.zero)

  def nodeFromInt(n: Int) = RationalNode(Rational(n))
  def nodeToInt(n: Node): Option[Int] = {
    val e = RationalTree.evaluator(RationalTree.this)
    val r: Rational = scala.util.Try(e.value(n.asInstanceOf[e.tree.Node])).getOrElse(sys.error(s"Node $n should be an integer"))
    if (r.isWhole) Some(r.toInt) else None
  }

  override def toCompact(displayNode: Node): Node = displayNode match {
    case r: RationalNode => r
    case _ => super.toCompact(displayNode)
  }

  override def toDisplay(compactNode: Node): Node = compactNode match {
    case RationalNode(r) if r < 0 => 
      NegNode(toDisplay(RationalNode(-r)))
    case RationalNode(r) if r.denominator != 1 =>
      DivNode(RationalNode(r.numerator), RationalNode(r.denominator))
    case r: RationalNode => r
    case _ => super.toDisplay(compactNode)
  }

  override def trySimplified(compactNode: Node): Option[Node] = {
    compactNode match {
      case NegNode(RationalNode(r)) => return Some(RationalNode(-r))
      case InvNode(RationalNode(r)) => return Some(RationalNode(r.reciprocal))
      case PlusNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
        val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
        val r = rational.map(_.asInstanceOf[RationalNode].r).reduceOption(_+_).getOrElse(Rational.zero)
        if (r != 0)
          return Some(PlusNode(nonRational :+ RationalNode(r)))
        else
          return Some(PlusNode(nonRational))
      case TimesNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
        val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
        val r = rational.map(_.asInstanceOf[RationalNode].r).reduceOption(_*_).getOrElse(Rational.one)
        if (r == 1)
          return Some(TimesNode(nonRational))
        return Some(r.signum match {
          case 0 => zeroNode
          case -1 => NegNode(TimesNode(RationalNode(-r) +: nonRational))
          case 1 => TimesNode(RationalNode(r) +: nonRational)
        })
      case _ =>
    }
    super.trySimplified(compactNode)
  }

  def TreeParser: RationalTreeParserTrait
  def TreeUnparser: RationalTreeUnparserTrait

  trait RationalTreeParserTrait extends FieldTreeParserTrait {
    lazy val integer: PackratParser[Node] =
      biginteger ^^ { case i => RationalNode(Rational(i)) }

    val biginteger = """0|([1-9]\d*)""".r ^^ { case s: String => BigInt(s) }
  }

  trait RationalTreeUnparserTrait extends FieldTreeUnparserTrait {
    override def printable(n: Node): Printable = n match {
      case RationalNode(r) =>
        require(r >= 0)
        require(r.denominator == 1)
        Value(r.toString)
      case _ => super.printable(n)
    }
  }
}

object RationalTree {
  def evaluator(rt: RationalTree): FieldEvaluator[RationalTree, Rational] =
    new FieldEvaluator[RationalTree, Rational] {
      val tree = rt
      implicit val scalarAlgebra = Rational.RationalAlgebra
      override def value(node: tree.Node): Rational = node match {
        case tree.RationalNode(r) => r
        case _ => super.value(node)
      }
    }
}

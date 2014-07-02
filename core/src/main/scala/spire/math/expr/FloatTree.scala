package spire.math
package expr

import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

trait FloatTree extends FieldTree {
  case class FloatNode(bd: BigDecimal, powerOfTen: Int) extends AtomNode

  override def toCompact(displayNode: Node): Node = displayNode match {
    case f: FloatNode => f
    case _ => super.toCompact(displayNode)
  }

  override def toDisplay(compactNode: Node): Node = compactNode match {
    case FloatNode(bd, powerOfTen) if bd < 0 =>
      NegNode(FloatNode(-bd, powerOfTen))
    case f: FloatNode => f
    case _ => super.toDisplay(compactNode)
  }

  override def trySimplified(compactNode: Node): Option[Node] = {
    compactNode match {
      case NegNode(FloatNode(bd, powerOfTen)) => return Some(FloatNode(-bd, powerOfTen))
      case _ =>
    }
    super.trySimplified(compactNode)
  }

  def TreeParser: FloatTreeParserTrait
  def TreeUnparser: FloatTreeUnparserTrait

  trait FloatTreeParserTrait extends FieldTreeParserTrait {
    lazy val float: PackratParser[Node] =
      (bigdecimal ~ opt(("e" | "E") ~> int))  ^^ { 
        case bd ~ None => FloatNode(bd, 0)
        case bd ~ Some(exp) => FloatNode(bd, exp)
      }

    val int = """-?(0|([1-9]\d*))""".r ^^ { case s: String => s.toInt }
    val bigdecimal = """(0|([1-9]\d*))?[.]\d*""".r ^^ { case s: String => BigDecimal(s) }
  }

  trait FloatTreeUnparserTrait extends FieldTreeUnparserTrait {
    // force printing the BigDecimal with a dot
    def printBigDecimal(bd: BigDecimal): String = bd.scale match {
      case 0 => bd.toString + "."
      case _ => bd.toString
    }

    override def printable(n: Node): Printable = n match {
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

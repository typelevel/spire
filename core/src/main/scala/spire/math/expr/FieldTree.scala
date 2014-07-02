package spire.math
package expr

import spire.algebra.{Ring, Field}
import spire.implicits._

trait FieldTree extends RingTree {
  case class DivNode(left: Node, right: Node) extends DisplayBinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => DivNode(newLeft, newRight)
    }
  }

  case class InvNode(node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => InvNode(n)
    }
    def isValidCompact = node.isValidCompact
    def isValidDisplay = false
  }

  override def toCompact(displayNode: Node): Node = displayNode match {
    case InvNode(node) => toCompact(node).reciprocal
    case DivNode(left, right) => toCompact(left) / toCompact(right)
    case _ => super.toCompact(displayNode)
  }

  override def toDisplay(compactNode: Node): Node = compactNode match {
    case InvNode(node) => DivNode(oneNode, toDisplay(node))
    case TimesNode(Seq(head, tail@_*)) => (toDisplay(head) /: tail) {
      case (displayNode, node) => toDisplay(node) match {
        case InvNode(n) => DivNode(displayNode, n)
        case n => MulNode(displayNode, n)
      }
    }
    case _ => super.toDisplay(compactNode)
  }

  override def trySimplified(compactNode: Node): Option[Node] = {
    compactNode match {
      case InvNode(InvNode(node)) => return Some(node)
      case InvNode(NegNode(node)) => return Some(NegNode(InvNode(node)))
      case InvNode(TimesNode(seq)) => return Some(TimesNode(seq.map(_.reciprocal)))
      case _ =>
    }
    super.trySimplified(compactNode)
  }

  class NodeField extends NodeRing with Field[Node] {
    def gcd(a: Node, b: Node): Node =
      sys.error("Not implemented")
    def mod(a: Node, b: Node): Node =
      sys.error("Not implemented")
    def quot(a: Node,b: Node): Node =
      sys.error("Not implemented")

    override def reciprocal(x: Node): Node = x match {
      case InvNode(node) => node
      case TimesNode(seq) => TimesNode(seq.map(reciprocal(_)))
      case _ => InvNode(x)
    }

    def div(x: Node, y: Node): Node = (x, y) match {
      case (TimesNode(a), TimesNode(b)) => TimesNode(a ++ b.map(_.reciprocal))
      case (TimesNode(a), b) => TimesNode(a :+ (b.reciprocal))
      case (a, TimesNode(b)) => TimesNode(a +: b.map(_.reciprocal))
      case (a, b) => TimesNode(Seq(a, b.reciprocal))
    }
  }

  def nodeToInt(node: Node): Option[Int]

  implicit def nodeField: Field[Node] = new NodeField

  def TreeParser: FieldTreeParserTrait
  def TreeUnparser: FieldTreeUnparserTrait

  trait FieldTreeParserTrait extends RingTreeParserTrait {
    lazy val d_op: PackratParser[Node] = ((m_expr <~ "/") ~ u_expr) ^^ { case x ~ y => DivNode(x, y) }
  }

  trait FieldTreeUnparserTrait extends RingTreeUnparserTrait {
    override def printable(n: Node): Printable = n match {
      case DivNode(left, right) => LeftAssoc(left, "/", right, priority = 20)
      case _ => super.printable(n)
    }
  }
}

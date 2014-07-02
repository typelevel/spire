package spire.math
package expr

import spire.algebra.{Field, Ring, Trig}
import spire.implicits._

trait TrigTree extends FieldTree {
  /** Function with a single argument. */
  case class FunNode(name: String, node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => FunNode(name, n)
    }
    def isValidCompact = node.isValidCompact
    def isValidDisplay = node.isValidDisplay
  }
  /** Atom for constants pi and e (and potientially others!). */
  case class ConstantNode(name: String) extends AtomNode

  /** A node representing left to the power right. */
  case class PowerNode(left: Node, right: Node) extends BinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => PowerNode(newLeft, newRight)
    }
    def isValidCompact = left.isValidCompact && right.isValidCompact
    def isValidDisplay = left.isValidDisplay && right.isValidDisplay
  }

  override def toCompact(displayNode: Node): Node = displayNode match {
    case _: ConstantNode => displayNode
    case FunNode(name, node) => FunNode(name, toCompact(node))
    case PowerNode(left, right) => nodeTrig.fpow(toCompact(left), toCompact(right))
    case _ => super.toCompact(displayNode)
  }

  override def toDisplay(compactNode: Node): Node = compactNode match {
    case _: ConstantNode => compactNode
    case FunNode(name, node) => FunNode(name, toDisplay(node))
    case PowerNode(left, right) => PowerNode(toDisplay(left), toDisplay(right))
    case _ => super.toDisplay(compactNode)
  }


  // no additional simplifications, is not overriding trySimplified

  class NodeTrig extends NodeField with Trig[Node] {
    def e = ConstantNode("e")
    def pi = ConstantNode("pi")

    def exp(node: Node) = FunNode("exp", node)
    def expm1(node: Node) = FunNode("exp", node - oneNode)
    def log(node: Node) = FunNode("log", node)
    def log1p(node: Node) = FunNode("log", oneNode + node)

    def sin(node: Node) = FunNode("sin", node)
    def cos(node: Node) = FunNode("cos", node)
    def tan(node: Node) = FunNode("tan", node)

    def asin(node: Node) = FunNode("asin", node)
    def acos(node: Node) = FunNode("acos", node)
    def atan(node: Node) = FunNode("atan", node)
    def atan2(y: Node, x: Node) = sys.error("TODO: not yet implemented")

    def sinh(node: Node) = FunNode("sinh", node)
    def cosh(node: Node) = FunNode("cosh", node)
    def tanh(node: Node) = FunNode("tanh", node)

    def toRadians(node: Node) = node / fromInt(180) * pi
    def toDegrees(node: Node) = node * fromInt(180) / pi

    def fpow(x: Node, y: Node) = PowerNode(x, y)
    def nroot(a: Node, n: Int) = PowerNode(a, fromInt(n).reciprocal)
  }

  implicit def nodeTrig = new NodeTrig

  def TreeParser: TrigTreeParserTrait
  def TreeUnparser: TrigTreeUnparserTrait

  trait TrigTreeParserTrait extends FieldTreeParserTrait {
    lazy val p_op: PackratParser[Node] =
      ((primary <~ "^") ~ u_expr) ^^ {
        case left ~ right => PowerNode(left, right)
      }

    lazy val call: PackratParser[Node] = identifier ~ (("(" ~> a_expr) <~ ")") ^^ {
      case name ~ node => FunNode(name, node)
    }

    def identifier = """[a-zA-z]+[0-9a-zA-z]*""".r
  }

  trait TrigTreeUnparserTrait extends FieldTreeUnparserTrait {
    override def printable(n: Node) = n match {
      case PowerNode(left, right) => RightAssoc(left, "^", right, priority = 40)
      case FunNode(name, node) => Enclosed(name + "(", node, ")")
      case _ => super.printable(n)
    }
  }
}

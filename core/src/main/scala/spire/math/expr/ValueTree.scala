package spire.math
package expr

import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

/** A trait for expression trees that can be parsed and pretty-printed. */
trait ValueTree[A] extends Tree {
  import spire.algebra.Order // {PartialOrder, Order}

  def atomValue(atom: AtomNode): A

  def value(node: Node): A = node match {
    case b: DisplayBinaryNode => sys.error(s"Node $b is in display format")
    case atom: AtomNode => atomValue(atom)
    case _ => sys.error(s"Cannot compute value of $node")
  }

  implicit def valueTreeOrder(implicit o: Order[A]) = new Order[Node] {
    def compare(x: Node, y: Node) = o.compare(value(x), value(y))
  }

  // waiting for PartialOrder merge
  /*  implicit def valueTreePartialOrder(implicit o: PartialOrder[A]) = new PartialOrder[Node] {
   def partialCompare(x: Node, y: Node) = o.partialCompare(value(x), value(y))
  } */
}

trait EuclideanRingValueTree[A] extends ValueTree[A] {
  implicit def euclideanRing: EuclideanRing[A]
  override def value(node: Node): A = node match {
    case PlusNode(seq) => seq.map(value(_)).reduce(_+_)
    case TimesNode(seq) => seq.map(value(_)).reduce(_*_)
    case NegNode(node) => -value(node)
    case _ => super.value(node)
  }
}

trait FieldValueTree[A] extends EuclideanRingValueTree[A] {
  implicit def field: Field[A]
  override def value(node: Node): A = node match {
    case InvNode(node) => value(node).reciprocal
    case _ => super.value(node)
  }
}

trait TrigValueTree[A] extends ValueTree[A] {
  implicit def trig: Trig[A]

  case class ConstantNode(name: String) extends AtomNode

  def fpow(left: A, right: A): A

  override def value(node: Node): A = node match {
    case ConstantNode("e") => trig.e
    case ConstantNode("pi") => trig.pi

    case PowerNode(left, right) => fpow(value(left), value(right))

    case FunNode("exp", node) => trig.exp(value(node))
    case FunNode("log", node) => trig.log(value(node))

    case FunNode("sin", node) => trig.sin(value(node))
    case FunNode("cos", node) => trig.cos(value(node))
    case FunNode("tan", node) => trig.tan(value(node))

    case FunNode("asin", node) => trig.asin(value(node))
    case FunNode("acos", node) => trig.acos(value(node))
    case FunNode("atan", node) => trig.atan(value(node))
      // atan2 not supported yet
    case FunNode("sinh", node) => trig.sinh(value(node))
    case FunNode("cosh", node) => trig.cosh(value(node))
    case FunNode("tanh", node) => trig.tanh(value(node))
    case _ => super.value(node)
  }
}

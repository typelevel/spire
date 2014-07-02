package spire.math
package expr

import spire.algebra.{Ring, Field, Trig, Order} //, PartialOrder}
import spire.implicits._

trait Evaluator[T <: Tree, A] {
  val tree: T
  def value(node: tree.Node): A = node match {
    case _: tree.DisplayBinaryNode => sys.error(s"Cannot evaluate display expression $node")
    case _ => sys.error(s"Unsupported expression $node")
  }

  implicit def nodeOrder(implicit ev: Order[A]): Order[tree.Node] = new Order[tree.Node] {
    def compare(x: tree.Node, y: tree.Node) = ev.compare(value(x), value(y))
  }

/*  implicit def nodePartialOrder[A: PartialOrder] = new PartialOrder[Node] {
 def partialCompare(x: Node, y: Node) = value(x).partialCompare(value(y))
 }*/
}

trait RingEvaluator[T <: RingTree, A] extends Evaluator[T, A] {
  val tree: T
  implicit val scalarAlgebra: Ring[A]

  override def value(node: tree.Node): A = node match {
    case tree.PlusNode(seq) => seq.map(value(_)).reduceOption(_+_).getOrElse(Ring[A].zero)
    case tree.TimesNode(seq) => seq.map(value(_)).reduceOption(_*_).getOrElse(Ring[A].one)
    case tree.NegNode(n) => -value(n)
    case _ => super.value(node)
  }
}

trait FieldEvaluator[T <: FieldTree, A] extends RingEvaluator[T, A] {
  val tree: T
  implicit val scalarAlgebra: Field[A]

  override def value(node: tree.Node): A = node match {
    case tree.InvNode(n) => value(n).reciprocal
    case _ => super.value(node)
  }
}

trait TrigEvaluator[T <: TrigTree, A] extends FieldEvaluator[T, A] {
  val tree: T
  implicit val scalarAlgebra: Trig[A] with Field[A]

  def fpow(base: A, exp: A): A

  override def value(node: tree.Node): A = node match {
    case tree.ConstantNode("e") => scalarAlgebra.e
    case tree.ConstantNode("pi") => scalarAlgebra.pi

    case tree.PowerNode(left, right) => fpow(value(left), value(right))

    case tree.FunNode("exp", node) => scalarAlgebra.exp(value(node))
    case tree.FunNode("log", node) => scalarAlgebra.log(value(node))

    case tree.FunNode("sin", node) => scalarAlgebra.sin(value(node))
    case tree.FunNode("cos", node) => scalarAlgebra.cos(value(node))
    case tree.FunNode("tan", node) => scalarAlgebra.tan(value(node))

    case tree.FunNode("asin", node) => scalarAlgebra.asin(value(node))
    case tree.FunNode("acos", node) => scalarAlgebra.acos(value(node))
    case tree.FunNode("atan", node) => scalarAlgebra.atan(value(node))
      // atan2 not supported yet
    case tree.FunNode("sinh", node) => scalarAlgebra.sinh(value(node))
    case tree.FunNode("cosh", node) => scalarAlgebra.cosh(value(node))
    case tree.FunNode("tanh", node) => scalarAlgebra.tanh(value(node))
    case _ => super.value(node)
  }
}

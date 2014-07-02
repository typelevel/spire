package spire.math

/** A trait for expression trees that can be parsed and pretty-printed. */
trait ValueTree[V] extends Tree {
  import spire.algebra.Order // {PartialOrder, Order}
  def value(node: Node): V

  implicit def valueTreeOrder(implicit o: Order[V]) = new Order[Node] {
    def compare(x: Node, y: Node) = o.compare(value(x), value(y))
  }

  // waiting for PartialOrder merge
  /*  implicit def valueTreePartialOrder(implicit o: PartialOrder[V]) = new PartialOrder[Node] {
   def partialCompare(x: Node, y: Node) = o.partialCompare(value(x), value(y))
  } */
}

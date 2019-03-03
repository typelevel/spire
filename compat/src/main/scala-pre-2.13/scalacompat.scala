package spire.scalacompat

trait ScalaOrderingWrapperCompat[A] extends scala.math.Ordering[A] {
  override def min(x:A, y:A): A = if (lt(x, y)) x else y
  override def max(x:A, y:A): A = if (gt(x, y)) x else y
}

trait BuilderCompat[-A, +To] extends scala.collection.mutable.Builder[A, To] {
  def addOne(elem: A): this.type
  def +=(elem: A): this.type = addOne(elem)
}

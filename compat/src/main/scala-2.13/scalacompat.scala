package spire.scalacompat

trait ScalaOrderingWrapperCompat[A] extends scala.math.Ordering[A] {
  override def min[U <: A](x:U, y:U): U = if (lt(x, y)) x else y
  override def max[U <: A](x:U, y:U): U = if (gt(x, y)) x else y
}

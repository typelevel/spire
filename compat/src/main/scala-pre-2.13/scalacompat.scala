package spire.scalacompat

trait ScalaOrderingWrapperCompat[A] extends scala.math.Ordering[A] {
  override def min(x:A, y:A): A = if (lt(x, y)) x else y
  override def max(x:A, y:A): A = if (gt(x, y)) x else y
}

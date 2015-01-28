package spire.util
 
trait NullboxVersions {
  def unapply[A](n: Nullbox[A]): Option[A] = n.toOption
}

object NullboxVersions {
  trait Base extends AnyRef {
    self: Nullbox[_] =>
    override def equals(other: Any): Boolean = scala2_10equals(other)
  }
}

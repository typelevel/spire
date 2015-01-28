package spire.util
 
trait NullboxVersions {
  def unapply[A](n: Nullbox[A]): Option[A] = n.toOption
}

object NullboxVersions {
  type Base = AnyRef
}

package spire
package util

trait OptVersions {
  def unapply[A](n: Opt[A]): Option[A] = n.toOption
}

object OptVersions {
  trait Base extends AnyRef {
    self: Opt[_] =>
    override def equals(other: Any): Boolean = scala2_10equals(other)
    override def hashCode: Int = scala2_10hashCode
  }
}

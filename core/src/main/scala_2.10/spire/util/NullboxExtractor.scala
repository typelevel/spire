package spire.util
 
trait NullboxExtractor {
  def unapply[A](n: Nullbox[A]): Option[A] = n.toOption
}

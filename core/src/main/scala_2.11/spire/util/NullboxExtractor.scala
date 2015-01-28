package spire.util
 
trait NullboxExtractor {
  // name-based extractor, cf. http://hseeberger.github.io/blog/2013/10/04/name-based-extractors-in-scala-2-dot-11/
  def unapply[A](n: Nullbox[A]): Nullbox[A] = n
}

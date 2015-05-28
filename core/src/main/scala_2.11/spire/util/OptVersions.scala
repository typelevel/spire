package spire.util

trait OptVersions {
  // name-based extractor, cf. http://hseeberger.github.io/blog/2013/10/04/name-based-extractors-in-scala-2-dot-11/
  def unapply[A](n: Opt[A]): Opt[A] = n
}

object OptVersions {
  type Base = AnyVal
}

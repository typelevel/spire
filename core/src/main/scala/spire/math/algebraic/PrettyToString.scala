package spire.math.algebraic


trait PrettyToString[A <: PrettyToString[A]] extends RealLike[A] { self: A =>
  def toTreeString: String = TreeString(this).toString
}


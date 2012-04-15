package spire.algebra


trait Group[A] extends Monoid[A] {
  def inverse(a: A): A
}

object Group {
  def apply[G](implicit G: Group[G]): Group[G] = G
}

class AdditiveGroup[A:Ring] extends Group[A] {
  private val ring = implicitly[Ring[A]]
  def identity = ring.zero
  def inverse(a: A): A = ring.negate(a)
  def op(x:A, y:A) = ring.plus(x, y)
}




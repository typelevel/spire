package numerics.math

trait Semigroup[A] extends Eq[A] {
  def op(x:A, y:A): A
}

trait SemigroupOps[A] {
  val lhs:A
  val s:Semigroup[A]
  def |+|(rhs:A) = s.op(lhs, rhs)
}

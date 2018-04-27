package spire.algebra

/** Describes a conjugation, which is an antihomomorphism. It obeys:
  *
  *   i. conjugate(1) = 1
  *  ii. conjugate(x + y) = conjugate(x) + conjugate(y)
  * iii. conjugate(x*y) = conjugate(y)*conjugate(x)
  *
  */
trait Conjugation[A] {
  def conjugate(a: A): A
}

object Conjugation {

  def apply[A](implicit ev: Conjugation[A]): Conjugation[A] = ev

  def selfConjugate[A]: Conjugation[A] = new Conjugation[A] {
    def conjugate(a: A): A = a
  }

  implicit def fromIsReal[A:IsReal]: Conjugation[A] = selfConjugate[A]

}

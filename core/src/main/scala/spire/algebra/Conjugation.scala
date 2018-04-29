package spire
package algebra

/** Describes a conjugation, which is an antihomomorphism. It obeys:
  *
  *   i. conjugate(1) = 1
  *  ii. conjugate(x + y) = conjugate(x) + conjugate(y)
  * iii. conjugate(x*y) = conjugate(y)*conjugate(x)
  *
  */
trait Conjugation[@sp(Int, Long, Float, Double) A] {
  def conjugate(a: A): A
}

object Conjugation {

  def apply[A](implicit ev: Conjugation[A]): Conjugation[A] = ev

  final class SelfConjugate[@sp(Int, Long, Float, Double) A] extends Conjugation[A] {
    def conjugate(a: A): A = a
  }

  def selfConjugate[@sp(Int, Long, Float, Double) A]: Conjugation[A] = new SelfConjugate[A]

  implicit def fromIsReal[@sp(Int, Long, Float, Double) A:IsReal]: Conjugation[A] = selfConjugate[A]

}

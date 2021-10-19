package spire
package algebra

/**
 * Describes an involution, which is an operator that satisfies f(f(x)) = x (rule i).
 *
 * If a multiplicative semigroup is available, it describes an antiautomorphism (rule ii); on a multiplicative monoid,
 * it preserves the identity (rule iii).
 *
 * If a ring is available, it should be compatible with addition (rule iv), and then defines a *-ring (see
 * https://en.wikipedia.org/wiki/%2A-algebra ).
 *
 *   i. adjoint(adjoint(x)) = x ii. adjoint(x*y) = adjoint(y)*adjoint(x) (with an underlying multiplicative semigroup)
 *      iii. adjoint(1) = 1 (with an underlying multiplicative monoid) iv. adjoint(x+y) = adjoint(x)+adjoint(y) (with an
 *      underlying ring)
 *
 * A *-algebra is an associative algebra A over a commutative *-ring R, where A has an involution as well. It satisfies,
 * for x: A, y: A and r: R
 *
 * v. adjoint(r * x + y) = adjoint(r)*adjoint(x) + adjoint(y)
 */
trait Involution[@sp(Int, Long, Float, Double) A] {

  def adjoint(a: A): A

}

object Involution {

  def apply[A](implicit ev: Involution[A]): Involution[A] = ev

  final class SelfAdjoint[@sp(Int, Long, Float, Double) A] extends Involution[A] {
    def adjoint(a: A): A = a
  }

  def selfAdjoint[@sp(Int, Long, Float, Double) A]: Involution[A] = new SelfAdjoint[A]

  implicit def fromIsReal[@sp(Int, Long, Float, Double) A: IsReal]: Involution[A] = selfAdjoint[A]

}

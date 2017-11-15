package spire
package laws

import spire.algebra._
import spire.algebra.lattice.Heyting

// TODO: add source for the laws
trait HeytingLaws[A] {
  implicit def A: Heyting[A]

  def andAssociative(x: A, y: A, z: A): IsEq[A] =
    A.and(A.and(x, y), z) <=> A.and(x, A.and(y, z))

  def andCommutatiev(x: A, y: A): IsEq[A] =
    A.and(x, y) <=> A.and(y, x)

  def andIdentity(x: A): IsEq[A] =
    A.and(x, A.one) <=> x

  def orAssociative(x: A, y: A, z: A): IsEq[A] =
    A.or(A.or(x, y), z) <=> A.or(x, A.or(y, z))

  def orCommutatiev(x: A, y: A): IsEq[A] =
    A.or(x, y) <=> A.or(y, x)

  def orIdentity(x: A): IsEq[A] =
    A.or(x, A.zero) <=> x

  def absorptionAndOr(x: A, y: A): IsEq[A] =
    A.and(x, A.or(x, y)) <=> x

  def absorptionOrAnd(x: A, y: A): IsEq[A] =
    A.or(x, A.and(x, y)) <=> x

  def distributiveAndOr(x: A, y: A, z: A): IsEq[A] =
    A.and(x, A.or(y, z)) <=> A.or(A.and(x, y), A.and(x, z))

  def distributiveOrAnd(x: A, y: A, z: A): IsEq[A] =
    A.or(x, A.and(y, z)) <=> A.and(A.or(x, y), A.or(x, z))

  def consistent(x: A): IsEq[A] =
    A.and(x, A.complement(x)) <=> A.zero

  def complementIsImplies(x: A): IsEq[A] =
    A.complement(x) <=> A.imp(x, A.zero)

  def selfImpliesIsOne(x: A): IsEq[A] =
    A.imp(x, x) <=> A.one

  // if x → y and y → x then x=y
  def bothImpliesIsEq(x: A, y: A, eqA: Eq[A]): IsEq[Boolean] =
    (eqA.neqv(A.imp(x, y), A.one) || eqA.neqv(A.imp(y, x), A.one) || eqA.eqv(x, y)) <=> true

  // if (1 → x)=1 then x=1
  def impliesOne(x: A, eqA: Eq[A]): IsEq[Boolean] =
    (eqA.neqv(A.imp(A.one, x), A.one) || eqA.eqv(x, A.one)) <=> true

  // x → (y → x) = 1
  def implies2(x: A, y: A): IsEq[A] =
    A.imp(x, A.imp(y, x)) <=> A.one

  // (x→(y→z)) → ((x→y)→(x→z)) = 1
  def implies3(x: A, y: A, z: A): IsEq[A] =
    A.imp(
      A.imp(x, A.imp(y, z)),
      A.imp(A.imp(x, y), A.imp(x, z))
    ) <=> A.one

  // x∧y → x = 1
  def andImpliesLeft(x: A, y: A): IsEq[A] =
    A.imp(A.and(x, y), x) <=> A.one

  // x∧y → y = 1
  def andImpliesRight(x: A, y: A): IsEq[A] =
    A.imp(A.and(x, y), y) <=> A.one

  // x → y → (x∧y) = 1
  def impliesImpliesAnd(x: A, y: A): IsEq[A] =
    A.imp(x, A.imp(y, A.and(x, y))) <=> A.one

  // x → x∨y
  def impliesOrLeft(x: A, y: A): IsEq[A] =
    A.imp(x, A.or(x, y)) <=> A.one

  // y → x∨y
  def impliesOrRight(x: A, y: A): IsEq[A] =
    A.imp(y, A.or(x, y)) <=> A.one

  // (x → z) → ((y → z) → ((x | y) → z)) = 1
  def impliesOr3(x: A, y: A, z: A): IsEq[A] =
    A.imp(
      A.imp(x, z),
      A.imp(
        A.imp(y, z),
        A.imp(A.or(x, y), z)
      )
    ) <=> A.one

  def zeroImplies(x: A): IsEq[A]=
    A.imp(A.zero, x) <=> A.one
}

trait BoolLaws[A] extends HeytingLaws[A] {
  implicit def A: Bool[A]

  def excludedMiddle(x: A): IsEq[A] =
    A.or(x, A.complement(x)) <=> A.one

  def xor(x: A, y: A): IsEq[A] =
    A.xor(x, y) <=> A.or(A.and(x, A.complement(y)), A.and(A.complement(x), y))

  def nxor(x: A, y: A): IsEq[A] =
    A.nxor(x, y) <=> A.and(A.or(x, A.complement(y)), A.or(A.complement(x), y))

  def imp(x: A, y: A): IsEq[A] =
    A.imp(x, y) <=> A.or(A.complement(x), y)

  def nand(x: A, y: A): IsEq[A] =
    A.nand(x, y) <=> A.complement(A.and(x, y))

  def nor(x: A, y: A): IsEq[A] =
    A.nor(x, y) <=> A.complement(A.or(x, y))
}

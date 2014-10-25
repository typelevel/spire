package spire.optional

import spire.algebra._
import spire.math._

object unicode {

  type ℍ = Quaternion[Real]
  type ℂ = Complex[Real]
  type ℝ = Real
  type ℚ = Rational
  type ℤ = SafeLong
  type ℕ = Natural

  val ⅇ = Real.e
  val π = Real.pi
  val φ = (Real(1) + Real(5).sqrt) / Real(2)
  val ⅈ = Complex.i[Real]
  val ⅉ = Quaternion.j[Real]

  def ⊤[A](implicit ev: Heyting[A]): A = ev.one
  def ⊥[A](implicit ev: Heyting[A]): A = ev.zero
  def ¬[A](a: A)(implicit ev: Heyting[A]): A = ev.complement(a)
  def √[A](a: A)(implicit ev: NRoot[A]): A = ev.sqrt(a)
  def ∛[A](a: A)(implicit ev: NRoot[A]): A = ev.nroot(a, 3)
  def ∜[A](a: A)(implicit ev: NRoot[A]): A = ev.nroot(a, 4)

  def Σ[A](as: Iterable[A])(implicit ev: AdditiveMonoid[A]): A =
    as.aggregate(ev.zero)(ev.plus, ev.plus)

  def Π[A](as: Iterable[A])(implicit ev: MultiplicativeMonoid[A]): A =
    as.aggregate(ev.one)(ev.times, ev.times)

  implicit class TimesOp[A](lhs: A)(implicit ev: MultiplicativeSemigroup[A]) {
    def ∙(rhs: A): A = ev.times(lhs, rhs)
  }

  implicit class EqOps[A](lhs: A)(implicit ev: Eq[A]) {
    def ≡(rhs: A): Boolean = ev.eqv(lhs, rhs)
    def ≠(rhs: A): Boolean = lhs != rhs
  }

  implicit class PartialOrderOps[A](lhs: A)(implicit ev: PartialOrder[A]) {
    def ≤(rhs: A): Boolean = ev.lteqv(lhs, rhs)
    def ≥(rhs: A): Boolean = ev.gteqv(lhs, rhs)
  }

  implicit class HeytingOps[A](lhs: A)(implicit ev: Heyting[A]) {
    def ∧(rhs: A): A = ev.and(lhs, rhs)
    def ∨(rhs: A): A = ev.or(lhs, rhs)
    def ⊃(rhs: A): A = ev.imp(lhs, rhs)
  }

  implicit class BoolOps[A](lhs: A)(implicit ev: Bool[A]) {
    def ⊻(rhs: A): A = ev.xor(lhs, rhs)
    def ⊼(rhs: A): A = ev.nand(lhs, rhs)
    def ⊽(rhs: A): A = ev.nor(lhs, rhs)
  }

  implicit class SymbolicIntervalOps[A](lhs: Interval[A]) {
  
    def ∋(rhs: A): Boolean = lhs contains rhs
    def ∌(rhs: A): Boolean = !(lhs contains rhs)
  
    def ∈:(a: A): Boolean = lhs contains a
    def ∉:(a: A): Boolean = !(lhs contains a)

    def ∩(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): Interval[A] =
      lhs intersect rhs
  
    def ∪(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): Interval[A] =
      lhs union rhs
  
    def \(rhs: Interval[A])(implicit r: AdditiveMonoid[A]): List[Interval[A]] =
      lhs -- rhs
  
    def ⊂(rhs: Interval[A]): Boolean = lhs isProperSubsetOf rhs
    def ⊃(rhs: Interval[A]): Boolean = lhs isProperSupersetOf rhs
  
    def ⊆(rhs: Interval[A]): Boolean = lhs isSubsetOf rhs
    def ⊇(rhs: Interval[A]): Boolean = lhs isSupersetOf rhs
  }


  implicit class SymbolicSetOps[A](lhs: Set[A]) {
    def ∋(rhs: A): Boolean = lhs contains rhs
    def ∌(rhs: A): Boolean = !(lhs contains rhs)
  
    def ∈:(a: A): Boolean = lhs contains a
    def ∉:(a: A): Boolean = !(lhs contains a)

    def ∩(rhs: Set[A]): Set[A] = lhs & rhs
    def ∪(rhs: Set[A]): Set[A] = lhs | rhs
    def \(rhs: Set[A]): Set[A] = lhs -- rhs
  
    def ⊂(rhs: Set[A]): Boolean = lhs.size < rhs.size && lhs.forall(rhs)
    def ⊃(rhs: Set[A]): Boolean = lhs.size > rhs.size && rhs.forall(lhs)
  
    def ⊆(rhs: Set[A]): Boolean = lhs.size <= rhs.size && lhs.forall(rhs)
    def ⊇(rhs: Set[A]): Boolean = lhs.size >= rhs.size && rhs.forall(lhs)
  }
}

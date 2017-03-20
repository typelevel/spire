package spire

package object algebra {

  type Homomorphism[A, B, F[_]] = Conversion[A, B] { type Hom[X] = F[X] }

  type Eq[A] = _root_.algebra.Eq[A]
  val Eq = _root_.algebra.Eq

  type PartialOrder[A] = _root_.algebra.PartialOrder[A]
  val PartialOrder = _root_.algebra.PartialOrder

  type Order[A] = _root_.algebra.Order[A]
  val Order = _root_.algebra.Order

  type Semigroup[A] = _root_.algebra.Semigroup[A]
  val Semigroup = _root_.algebra.Semigroup

  type CSemigroup[A] = _root_.algebra.CommutativeSemigroup[A]
  val CSemigroup = _root_.algebra.CommutativeSemigroup

  type Monoid[A] = _root_.algebra.Monoid[A]
  val Monoid = _root_.algebra.Monoid

  type CMonoid[A] = _root_.algebra.CommutativeMonoid[A]
  val CMonoid = _root_.algebra.CommutativeMonoid

  type Group[A] = _root_.algebra.Group[A]
  val Group = _root_.algebra.Group

  type AbGroup[A] = _root_.algebra.CommutativeGroup[A]
  val AbGroup = _root_.algebra.CommutativeGroup

  type AdditiveSemigroup[A] = _root_.algebra.ring.AdditiveSemigroup[A]
  val AdditiveSemigroup = _root_.algebra.ring.AdditiveSemigroup

  type AdditiveCSemigroup[A] = _root_.algebra.ring.AdditiveCommutativeSemigroup[A]
  val AdditiveCSemigroup = _root_.algebra.ring.AdditiveCommutativeSemigroup

  type AdditiveMonoid[A] = _root_.algebra.ring.AdditiveMonoid[A]
  val AdditiveMonoid = _root_.algebra.ring.AdditiveMonoid

  type AdditiveCMonoid[A] = _root_.algebra.ring.AdditiveCommutativeMonoid[A]
  val AdditiveCMonoid = _root_.algebra.ring.AdditiveCommutativeMonoid

  type AdditiveGroup[A] = _root_.algebra.ring.AdditiveGroup[A]
  val AdditiveGroup = _root_.algebra.ring.AdditiveGroup

  type AdditiveAbGroup[A] = _root_.algebra.ring.AdditiveCommutativeGroup[A]
  val AdditiveAbGroup = _root_.algebra.ring.AdditiveCommutativeGroup

  object Additive {
    def apply[A](s: Semigroup[A]): AdditiveSemigroup[A] = new AdditiveSemigroup[A] {
      def plus(x: A, y: A): A = s.combine(x, y)
    }

    def apply[A](s: CSemigroup[A]): AdditiveCSemigroup[A] = new AdditiveCSemigroup[A] {
      def plus(x: A, y: A): A = s.combine(x, y)
    }

    def apply[A](m: Monoid[A]): AdditiveMonoid[A] = new AdditiveMonoid[A] {
      def plus(x: A, y: A): A = m.combine(x, y)
      def zero = m.empty
    }

    def apply[A](m: CMonoid[A]): AdditiveCMonoid[A] = new AdditiveCMonoid[A] {
      def plus(x: A, y: A): A = m.combine(x, y)
      def zero = m.empty
    }

    def apply[A](g: Group[A]): AdditiveGroup[A] = new AdditiveGroup[A] {
      def plus(x: A, y: A): A = g.combine(x, y)
      override def minus(x: A, y: A): A = g.remove(x, y)
      def zero: A = g.empty
      def negate(x: A): A = g.inverse(x)
    }

    def apply[A](g: AbGroup[A]): AdditiveAbGroup[A] = new AdditiveAbGroup[A] {
      def plus(x: A, y: A): A = g.combine(x, y)
      override def minus(x: A, y: A): A = g.remove(x, y)
      def zero: A = g.empty
      def negate(x: A): A = g.inverse(x)
    }
  }

  type MultiplicativeSemigroup[A] = _root_.algebra.ring.MultiplicativeSemigroup[A]
  val MultiplicativeSemigroup = _root_.algebra.ring.MultiplicativeSemigroup

  type MultiplicativeCSemigroup[A] = _root_.algebra.ring.MultiplicativeCommutativeSemigroup[A]
  val MultiplicativeCSemigroup = _root_.algebra.ring.MultiplicativeCommutativeSemigroup

  type MultiplicativeMonoid[A] = _root_.algebra.ring.MultiplicativeMonoid[A]
  val MultiplicativeMonoid = _root_.algebra.ring.MultiplicativeMonoid

  type MultiplicativeCMonoid[A] = _root_.algebra.ring.MultiplicativeCommutativeMonoid[A]
  val MultiplicativeCMonoid = _root_.algebra.ring.MultiplicativeCommutativeMonoid

  type MultiplicativeGroup[A] = _root_.algebra.ring.MultiplicativeGroup[A]
  val MultiplicativeGroup = _root_.algebra.ring.MultiplicativeGroup

  type MultiplicativeAbGroup[A] = _root_.algebra.ring.MultiplicativeCommutativeGroup[A]
  val MultiplicativeAbGroup = _root_.algebra.ring.MultiplicativeCommutativeGroup

  object Multiplicative {
    def apply[A](s: Semigroup[A]): MultiplicativeSemigroup[A] = new MultiplicativeSemigroup[A] {
      def times(x: A, y: A): A = s.combine(x, y)
    }

    def apply[A](s: CSemigroup[A]): MultiplicativeCSemigroup[A] = new MultiplicativeCSemigroup[A] {
      def times(x: A, y: A): A = s.combine(x, y)
    }

    def apply[A](m: Monoid[A]): MultiplicativeMonoid[A] = new MultiplicativeMonoid[A] {
      def times(x: A, y: A): A = m.combine(x, y)
      def one = m.empty
    }

    def apply[A](m: CMonoid[A]): MultiplicativeCMonoid[A] = new MultiplicativeCMonoid[A] {
      def times(x: A, y: A): A = m.combine(x, y)
      def one = m.empty
    }

    def apply[A](g: Group[A]): MultiplicativeGroup[A] = new MultiplicativeGroup[A] {
      def times(x: A, y: A): A = g.combine(x, y)
      def div(x: A, y: A): A = g.remove(x, y)
      def one: A = g.empty
      override def reciprocal(x: A): A = g.inverse(x)
    }

    def apply[A](g: AbGroup[A]): MultiplicativeAbGroup[A] = new MultiplicativeAbGroup[A] {
      def times(x: A, y: A): A = g.combine(x, y)
      def div(x: A, y: A): A = g.remove(x, y)
      def one: A = g.empty
      override def reciprocal(x: A): A = g.inverse(x)
    }
  }

  type Semiring[A] = _root_.algebra.ring.Semiring[A]
  val Semiring = _root_.algebra.ring.Semiring

  type CSemiring[A] = _root_.algebra.ring.CommutativeSemiring[A]
  val CSemiring = _root_.algebra.ring.CommutativeSemiring

  type Rig[A] = _root_.algebra.ring.Rig[A]
  val Rig = _root_.algebra.ring.Rig

  type CRig[A] = _root_.algebra.ring.CommutativeRig[A]
  val CRig = _root_.algebra.ring.CommutativeRig

  type Rng[A] = _root_.algebra.ring.Rng[A]
  val Rng = _root_.algebra.ring.Rng

  type CRng[A] = _root_.algebra.ring.CommutativeRng[A]
  val CRng = _root_.algebra.ring.CommutativeRng

  type Ring[A] = _root_.algebra.ring.Ring[A]
  val Ring = _root_.algebra.ring.Ring

  type CRing[A] = _root_.algebra.ring.CommutativeRing[A]
  val CRing = _root_.algebra.ring.CommutativeRing

  type AlgebraField[A] = _root_.algebra.ring.Field[A]
  val AlgebraField = _root_.algebra.ring.Field

  type Bool[A] = _root_.algebra.lattice.Bool[A]
  val Bool = _root_.algebra.lattice.Bool
}

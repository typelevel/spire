package spire.laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.{Laws, Predicate}

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object RingLaws {
  def apply[A : Eq : Arbitrary](implicit _pred: Predicate[A]) = new RingLaws[A] {
    def Arb = implicitly[Arbitrary[A]]
    def pred = _pred
    val nonZeroLaws = new GroupLaws[A] {
      def Arb = Arbitrary(arbitrary[A] filter _pred)
      def Equ = Eq[A]
    }
  }
}

trait RingLaws[A] extends GroupLaws[A] {

  // must be a val (stable identifier)
  val nonZeroLaws: GroupLaws[A]
  def pred: Predicate[A]

  def withPred(_pred: Predicate[A], replace: Boolean = true): RingLaws[A] = RingLaws[A](
    Equ,
    Arb,
    if (replace) _pred else pred && _pred
  )

  implicit def Arb: Arbitrary[A]
  implicit def Equ: Eq[A] = nonZeroLaws.Equ


  // multiplicative groups

  def multiplicativeSemigroup(implicit A: MultiplicativeSemigroup[A]) = new MultiplicativeProperties(
    base = _.semigroup(A.multiplicative),
    parent = None,
    "prodn(a, 1) === a" → forAll((a: A) =>
      A.prodn(a, 1) === a
    ),
    "prodn(a, 2) === a * a" → forAll((a: A) =>
      A.prodn(a, 2) === (a * a)
    )
  )

  def multiplicativeMonoid(implicit A: MultiplicativeMonoid[A]) = new MultiplicativeProperties(
    base = _.monoid(A.multiplicative),
    parent = Some(multiplicativeSemigroup),
    "prodn(a, 0) === one" → forAll((a: A) =>
      A.prodn(a, 0) === A.one
    ),
    "prod(Nil) === one" → forAll((a: A) =>
      A.prod(Nil) === A.one
    ),
    "isOne" → forAll((a: A) =>
      a.isOne === (a === A.one)
    )
  )

  def multiplicativeGroup(implicit A: MultiplicativeGroup[A]) = new MultiplicativeProperties(
    base = _.group(A.multiplicative),
    parent = Some(multiplicativeMonoid),
    "reciprocal consistent" → forAll((x: A) =>
      pred(x) ==> ((A.one / x) === x.reciprocal)
    )
  )

  def multiplicativeAbGroup(implicit A: MultiplicativeAbGroup[A]) = new MultiplicativeProperties(
    base = _.abGroup(A.multiplicative),
    parent = Some(multiplicativeGroup)
  )


  // rings

  def semiring(implicit A: Semiring[A]) = new RingProperties(
    name = "semiring",
    al = additiveSemigroup,
    ml = multiplicativeSemigroup,
    parents = Seq.empty,
    "distributive" → forAll((x: A, y: A, z: A) =>
      (x * (y + z) === (x * y + x * z)) && (((x + y) * z) === (x * z + y * z))
    ),
    "pow" → forAll((x: A) =>
      ((x pow 1) === x) && ((x pow 2) === x * x) && ((x pow 3) === x * x * x)
    )
  )

  def rng(implicit A: Rng[A]) = new RingProperties(
    name = "rng",
    al = additiveAbGroup,
    ml = multiplicativeSemigroup,
    parents = Seq(semiring)
  )

  def rig(implicit A: Rig[A]) = new RingProperties(
    name = "rig",
    al = additiveMonoid,
    ml = multiplicativeMonoid,
    parents = Seq(semiring)
  )

  def ring(implicit A: Ring[A]) = new RingProperties(
    // TODO fromParents
    name = "ring",
    al = additiveAbGroup,
    ml = multiplicativeMonoid,
    parents = Seq(rig, rng)
  )

  def euclideanRing(implicit A: EuclideanRing[A]) = RingProperties.fromParent(
    // TODO tests?!
    name = "euclidean ring",
    parent = ring
  )

  // Everything below fields (e.g. rings) does not require their multiplication
  // operation to be a group. Hence, we do not check for the existence of an
  // inverse. On the other hand, fields require their multiplication to be an
  // abelian group. No we have to worry about zero.
  // The usual text book definition says: Fields consist of two abelian groups
  // (set, +, zero) and (set \ zero, *, one). We do the same thing here.
  // However, since law checking for the multiplication does not include zero
  // any more, it is not immediately clear that desired properties like
  // zero * x == x * zero hold.
  // Luckily, these follow from the other field and group axioms.
  def field(implicit A: Field[A]) = new RingProperties(
    name = "field",
    al = additiveAbGroup,
    ml = multiplicativeAbGroup,
    parents = Seq(euclideanRing)
  ) {
    override def nonZero = true
  }


  // property classes

  class MultiplicativeProperties(
    val base: GroupLaws[A] => GroupLaws[A]#GroupProperties,
    val parent: Option[MultiplicativeProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    private val _base = base(RingLaws.this)

    val name = _base.name
    val bases = Seq("base" → _base)
  }

  object RingProperties {
    def fromParent(name: String, parent: RingProperties, props: (String, Prop)*) =
      new RingProperties(name, parent.al, parent.ml, Seq(parent), props: _*)
  }

  class RingProperties(
    val name: String,
    val al: AdditiveProperties,
    val ml: MultiplicativeProperties,
    val parents: Seq[RingProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    def nonZero: Boolean = false

    def _ml =
      if (nonZero)
        new RuleSet with HasOneParent {
          val name = ml.name
          val bases = Seq("base-nonzero" → ml.base(nonZeroLaws))
          val parent = ml.parent
          val props = ml.props
        }
      else
        ml

    def bases = Seq("additive" → al, "multiplicative" → _ml)
  }

}

// vim: expandtab:ts=2:sw=2

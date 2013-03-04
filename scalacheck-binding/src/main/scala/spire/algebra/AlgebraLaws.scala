package spire.algebra

import spire.implicits._

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

object AlgebraLaws {
  def apply[A : Eq : Arbitrary] = new AlgebraLaws[A] {
    def Eq = implicitly[Eq[A]]
    def Arbitrary = implicitly[Arbitrary[A]]
  }
}

trait AlgebraLaws[A] extends Laws {

  implicit def Eq: Eq[A]
  implicit def Arbitrary: Arbitrary[A]


  // groups

  def semigroup(implicit A: Semigroup[A]) = new GroupProperties(
    name = "semigroup",
    parent = None,
    "associative" → forAll((x: A, y: A, z: A) =>
      ((x |+| y) |+| z) === (x |+| (y |+| z))
    )
  )

  def monoid(implicit A: Monoid[A]) = new GroupProperties(
    name = "monoid",
    parent = Some(semigroup),
    "left identity" → forAll((x: A) =>
      (A.id |+| x) === x
    ),
    "right identity" → forAll((x: A) =>
      (x |+| A.id) === x
    )
  )

  def group(implicit A: Group[A]) = new GroupProperties(
    name = "group",
    parent = Some(monoid),
    "left inverse" → forAll((x: A) =>
      A.id === (x.inverse |+| x)
    ),
    "right inverse" → forAll((x: A) =>
      A.id === (x |+| x.inverse)
    )
  )

  def abGroup(implicit A: AbGroup[A]) = new GroupProperties(
    name = "abelian group",
    parent = Some(group),
    "commutative" → forAll((x: A, y: A) =>
      (x |+| y) === (y |+| x)
    )
  )


  // additive groups

  def additiveSemigroup(implicit A: AdditiveSemigroup[A]) = new AdditiveProperties(
    base = semigroup(A.additive),
    parent = None
  )

  def additiveMonoid(implicit A: AdditiveMonoid[A]) = new AdditiveProperties(
    base = monoid(A.additive),
    parent = Some(additiveSemigroup)
  )

  def additiveGroup(implicit A: AdditiveGroup[A]) = new AdditiveProperties(
    base = group(A.additive),
    parent = Some(additiveMonoid),
    "minus consistent" → forAll((x: A, y: A) =>
      (x - y) === (x + (-y))
    )
  )

  def additiveAbGroup(implicit A: AdditiveAbGroup[A]) = new AdditiveProperties(
    base = abGroup(A.additive),
    parent = Some(additiveGroup)
  )


  // multiplicative groups

  def multiplicativeSemigroup(implicit A: MultiplicativeSemigroup[A]) = new MultiplicativeProperties(
    base = semigroup(A.multiplicative),
    parent = None
  )

  def multiplicativeMonoid(implicit A: MultiplicativeMonoid[A]) = new MultiplicativeProperties(
    base = monoid(A.multiplicative),
    parent = Some(multiplicativeSemigroup)
  )

  def multiplicativeGroup(implicit A: MultiplicativeGroup[A]) = new MultiplicativeProperties(
    base = group(A.multiplicative),
    parent = Some(multiplicativeMonoid),
    "reciprocal consistent" → forAll((x: A) =>
      (A.one / x) === x.reciprocal
    )
  )

  def multiplicativeAbGroup(implicit A: MultiplicativeAbGroup[A]) = new MultiplicativeProperties(
    base = abGroup(A.multiplicative),
    parent = Some(multiplicativeGroup),
    "commutative" → forAll((x: A, y: A) =>
      (x * y) === (y * x)
    )
  )


  // rings

  def semiring(implicit A: Semiring[A]) = new RingProperties(
    name = "semiring",
    additive = additiveSemigroup,
    multiplicative = multiplicativeSemigroup,
    parents = Seq.empty,
    "distributive" → forAll((x: A, y: A, z: A) =>
      (x * (y + z) === (x * y + x * z)) && (((x + y) * z) === (x * z + y * z))
    )
  )

  def rng(implicit A: Rng[A]) = new RingProperties(
    name = "rng",
    additive = additiveAbGroup,
    multiplicative = multiplicativeSemigroup,
    parents = Seq(semiring)
  )

  def rig(implicit A: Rig[A]) = new RingProperties(
    name = "rig",
    additive = additiveMonoid,
    multiplicative = multiplicativeMonoid,
    parents = Seq(semiring)
  )

  def ring(implicit A: Ring[A]) = new RingProperties(
    // TODO fromParents
    name = "ring",
    additive = additiveAbGroup,
    multiplicative = multiplicativeMonoid,
    parents = Seq(rig, rng)
  )

  def euclideanRing(implicit A: EuclideanRing[A]) = RingProperties.fromParent(
    // TODO tests?!
    name = "euclidean ring",
    parent = ring
  )

  def field(implicit A: Field[A]) = new RingProperties(
    name = "field",
    additive = additiveAbGroup,
    multiplicative = multiplicativeAbGroup,
    parents = Seq(euclideanRing)
  )


  // others

  def signed(implicit A: Signed[A]) = new SimpleProperties(
    name = "signed",
    // TODO != Negative ?!
    "abs non-negative" → forAll((x: A) =>
      x.abs != Negative
    )
  )


  // property classes

  class GroupProperties(name: String, parent: Option[GroupProperties], props: (String, Prop)*) extends DefaultProperties(name, parent, props: _*)

  class AdditiveProperties(val base: GroupProperties, val parent: Option[AdditiveProperties], val props: (String, Prop)*) extends SpireProperties with WithOneParent {
    val name = base.name
    val bases = Seq("base" → base)
  }

  class MultiplicativeProperties(val base: GroupProperties, val parent: Option[MultiplicativeProperties], val props: (String, Prop)*) extends SpireProperties with WithOneParent {
    val name = base.name
    val bases = Seq("base" → base)
  }

  object RingProperties {
    def fromParent(name: String, parent: RingProperties, props: (String, Prop)*) =
      new RingProperties(name, parent.additive, parent.multiplicative, Seq(parent), props: _*)
  }

  class RingProperties(val name: String, val additive: AdditiveProperties, val multiplicative: MultiplicativeProperties, val parents: Seq[RingProperties], val props: (String, Prop)*) extends SpireProperties {
    val bases = Seq("additive" → additive, "multiplicative" → multiplicative)
  }

}

// vim: expandtab:ts=2:sw=2

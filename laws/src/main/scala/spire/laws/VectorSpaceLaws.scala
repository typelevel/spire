package spire
package laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.{Laws, Predicate}

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object VectorSpaceLaws {
  def apply[V: Eq: Arbitrary, A: Eq: Arbitrary: Predicate] = new VectorSpaceLaws[V, A] {
    val scalarLaws = RingLaws[A]
    val vectorLaws = GroupLaws[V]
  }
}

trait VectorSpaceLaws[V, A] extends Laws {

  implicit def ringFromLeftModule[A](implicit V: LeftModule[_, A], ev: NoImplicit[CModule[_, A]]): Ring[A] = V.scalar
  implicit def ringFromRightModule[A](implicit V: RightModule[_, A], ev: NoImplicit[CModule[_, A]]): Ring[A] = V.scalar
  implicit def cRingFromCModule(implicit V: CModule[V, A]): CRing[A] = V.scalar

  val scalarLaws: RingLaws[A]
  val vectorLaws: GroupLaws[V]

  import scalarLaws.{ Equ => EqA, Arb => ArA }
  import vectorLaws.{ Equ => EqV, Arb => ArV }

  def leftModule(implicit V: LeftModule[V, A]) = new SpaceProperties(
    name = "leftModule",
    sl = _.ring(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq.empty,
    "left scalar distributes" -> forAll((r: A, v: V, w: V) =>
      r *: (v + w) === (r *: v) + (r *: w)
    ),
    "left vector distributes" -> forAll((r: A, s: A, v: V) =>
      (r + s) *: v === (r *: v) + (s *: v)
    ),
    "left associative scalar" -> forAll((r: A, s: A, v: V) =>
      (r * s) *: v === r *: (s *: v)
    ),
    "left identity" -> forAll((v: V) =>
      V.scalar.one *: v === v
    )
  )

  def rightModule(implicit V: RightModule[V, A]) = new SpaceProperties(
    name = "rightModule",
    sl = _.ring(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq.empty,
    "right scalar distributes" -> forAll((v: V, w: V, r: A) =>
      (v + w) :* r === (v :* r) + (w :* r)
    ),
    "right vector distributes" -> forAll((v: V, r: A, s: A) =>
      v :* (r + s) === (v :* r) + (v :* s)
    ),
    "right associative scalar" -> forAll((v: V, r: A, s: A) =>
      v :* (r * s) === (v :* r) :* s
    ),
    "right identity" -> forAll((v: V) =>
       v :* V.scalar.one === v
    )
  )

  def cModule(implicit V: CModule[V, A]): SpaceProperties = new SpaceProperties(
    name = "cModule",
    sl = _.cRing(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(leftModule, rightModule),
    "left and right multiplication are compatible" -> forAll((r: A, v: V, s: A) =>
    r *: (v :* s) === (r *: v) :* s
    )
  )

  def vectorSpace(implicit V: VectorSpace[V, A]): SpaceProperties = new SpaceProperties(
    name = "vector space",
    sl = _.field(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(cModule)
  )

  def metricSpace(implicit V: MetricSpace[V, A], o: Order[A], A: AdditiveMonoid[A]): SpaceProperties = new SpaceProperties(
    name = "metric space",
    sl = _.emptyRuleSet,
    vl = _.emptyRuleSet,
    parents = Seq.empty,
    "identity" → forAll((x: V, y: V) =>
      if (x === y) V.distance(x, y) === A.zero
      else V.distance(x, y) =!= A.zero
    ),
    "symmetric" → forAll((x: V, y: V) =>
      V.distance(x, y) === V.distance(y, x)
    ),
    "triangle inequality" → forAll((x: V, y: V, z: V) =>
      V.distance(x, z) <= (V.distance(x, y) + V.distance(y, z))
    )
  )

  def normedVectorSpace(implicit V: NormedVectorSpace[V, A], ev0: Order[A], ev1: Signed[A]): SpaceProperties = new SpaceProperties(
    name = "normed vector space",
    sl = _.field(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(vectorSpace, metricSpace),

    "scalable" → forAll((a: A, v: V) =>
      a.abs * v.norm === (a.abs *: v).norm
    ),
    "only 1 zero" → forAll((v: V) => // This is covered by metricSpace...
      if (v === V.zero)
        v.norm === Rng[A].zero
      else
        v.norm > Rng[A].zero
    )
  )

  def linearity(f: V => A)(implicit V: CModule[V, A]): SpaceProperties = new SimpleRuleSet(
    name = "linearity",

    "homogeneity" → forAll((r: A, v: V) =>
      f(r *: v) === r * f(v)
    ),
    "additivity" → forAll((v: V, w: V) =>
      f(v + w) === f(v) + f(w)
    )
  )


  def innerProductSpace(implicit V: InnerProductSpace[V, A], A: Order[A], A0: Signed[A]): SpaceProperties = SpaceProperties.fromParent(
    name = "inner-product space",
    parent = vectorSpace,

    "symmetry" → forAll((v: V, w: V) =>
      (v ⋅ w).abs === (w ⋅ v).abs
    ),
    "linearity of partial inner product" → forAll((w: V) =>
      // TODO this probably requires some thought -- should `linearity` be a full `RuleSet`?
      propertiesToProp(linearity(_ ⋅ w).all)
    )
  )

  object SpaceProperties {
    def fromParent(name: String, parent: SpaceProperties, props: (String, Prop)*) =
      new SpaceProperties(name, parent.sl, parent.vl, Seq(parent), props: _*)
  }

  class SpaceProperties(
    val name: String,
    val sl: scalarLaws.type => scalarLaws.RuleSet,
    val vl: vectorLaws.type => vectorLaws.RuleSet,
    val parents: Seq[SpaceProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq("scalar" → sl(scalarLaws), "vector" → vl(vectorLaws))
  }

}

// vim: expandtab:ts=2:sw=2

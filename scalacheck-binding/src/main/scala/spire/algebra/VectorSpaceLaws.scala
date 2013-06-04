package spire.algebra

import spire.implicits._

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object VectorSpaceLaws {
  def apply[V: Eq: Arbitrary, A: Eq: Arbitrary: Predicate] = new VectorSpaceLaws[V, A] {
    val scalarLaws = RingLaws[A]
    val vectorLaws = GroupLaws[V]
  }
}

trait VectorSpaceLaws[V, A] extends Laws {

  implicit def scalar(implicit V: Module[V, A]): Ring[A] = V.scalar

  val scalarLaws: RingLaws[A]
  val vectorLaws: GroupLaws[V]

  import scalarLaws.{ Equ => EqA, Arb => ArA }
  import vectorLaws.{ Equ => EqV, Arb => ArV }


  def module(implicit V: Module[V, A]) = new SpaceProperties(
    name = "module",
    sl = _.ring(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq.empty,

    "associative scalar" → forAll { (r: A, s: A, v: V) =>
      // TODO compiler crash if variable 'w' is replaced by its value
      val w = r *: s *: v
      w === ((r * s) *: v)
    },
    "scalar distributes over vector" → forAll((r: A, v: V, w: V) =>
      (r *: (v + w)) === ((r *: v) + (r *: w))
    ),
    "vector distributes over scalar" → forAll((r: A, s: A, v: V) =>
      ((r + s) *: v) === ((r *: v) + (s *: v))
    ),
    "scalar identity is identity" → forAll((v: V) =>
      (V.scalar.one *: v) === v
    )
  )

  def vectorSpace(implicit V: VectorSpace[V, A]) = new SpaceProperties(
    name = "vector space",
    sl = _.field(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(module)
  )

  def metricSpace(implicit V: MetricSpace[V, A], o: Order[A], A: Ring[A]) = new SpaceProperties(
    name = "metric space",
    sl = _.emptyProperties,
    vl = _.emptyProperties,
    parents = Seq.empty,
    "identity" → forAll((x: V, y: V) =>
      if (x === y) V.distance(x, y) === Ring[A].zero
      else V.distance(x, y) =!= Ring[A].zero
    ),
    "symmetric" → forAll((x: V, y: V) =>
      V.distance(x, y) === V.distance(y, x)
    ),
    "triangle inequality" → forAll((x: V, y: V, z: V) =>
      V.distance(x, z) <= (V.distance(x, y) + V.distance(y, z))
    )
  )

  def normedVectorSpace(implicit V: NormedVectorSpace[V, A], ev0: Order[A], ev1: Signed[A]) = new SpaceProperties(
    name = "normed vector space",
    sl = _.field(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(vectorSpace, metricSpace),

    "scalable" → forAll((a: A, v: V) =>
      a.abs * v.norm === (a.abs *: v).norm
    ),
    "only 1 zero" → forAll((v: V) => // This is covered by metricSpace...
      if (v === V.zero)
        v.norm === Ring[A].zero
      else
        v.norm > Ring[A].zero
    )
  )

  def linearity(f: V => A)(implicit V: Module[V, A]) = new SimpleProperties(
    name = "linearity",

    "homogeneity" → forAll((r: A, v: V) =>
      f(r *: v) === r * f(v)
    ),
    "additivity" → forAll((v: V, w: V) =>
      f(v + w) === f(v) + f(w)
    )
  )

  def innerProductSpace(implicit V: InnerProductSpace[V, A], A: Order[A], A0: Signed[A]) = SpaceProperties.fromParent(
    name = "inner-product space",
    parent = vectorSpace,

    "symmetry" → forAll((v: V, w: V) =>
      (v ⋅ w).abs === (w ⋅ v).abs
    ),
    "linearity of partial inner product" → forAll((w: V) =>
      linearity(_ ⋅ w)
    )
  )

  object SpaceProperties {
    def fromParent(name: String, parent: SpaceProperties, props: (String, Prop)*) =
      new SpaceProperties(name, parent.sl, parent.vl, Seq(parent), props: _*)
  }

  class SpaceProperties(
    val name: String,
    val sl: scalarLaws.type => scalarLaws.SpireProperties,
    val vl: vectorLaws.type => vectorLaws.SpireProperties,
    val parents: Seq[SpaceProperties],
    val props: (String, Prop)*
  ) extends SpireProperties {
    val bases = Seq("scalar" → sl(scalarLaws), "vector" → vl(vectorLaws))
  }

}

// vim: expandtab:ts=2:sw=2

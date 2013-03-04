package spire.algebra

import spire.implicits._

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

object VectorSpaceLaws {
  def apply[V: Eq: Arbitrary, A: Eq: Arbitrary] = new VectorSpaceLaws[V, A] {
    val vectorLaws = AlgebraLaws[V]
    val scalarLaws = AlgebraLaws[A]
  }
}

trait VectorSpaceLaws[V, A] extends Laws {

  implicit def scalar(implicit V: Module[V, A]): Ring[A] = V.scalar

  val scalarLaws: AlgebraLaws[A]
  val vectorLaws: AlgebraLaws[V]

  import scalarLaws.{ Eq => EqA, Arbitrary => ArbA }
  import vectorLaws.{ Eq => EqV, Arbitrary => ArbV }


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

  // TODO metric space dummy
  def metricSpace(implicit V: MetricSpace[V, A]) = new SpaceProperties(
    name = "metric space",
    sl = _.emptyProperties,
    vl = _.emptyProperties,
    parents = Seq.empty
  )

  def normedVectorSpace(implicit V: NormedVectorSpace[V, A], ev0: Order[A], ev1: Signed[A]) = new SpaceProperties(
    name = "normed vector space",
    sl = _.field(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(vectorSpace, metricSpace),

    "scalable" → forAll((a: A, v: V) =>
      a.abs * v.norm === (a.abs *: v).norm
    ),
    "triangle inequality" → forAll((v: V, w: V) =>
      (v + w).norm <= (v.norm + w.norm)
    ),
    "only 1 zero" → forAll((v: V) =>
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

  class SpaceProperties(val name: String, val sl: scalarLaws.type => scalarLaws.SpireProperties, val vl: vectorLaws.type => vectorLaws.SpireProperties, val parents: Seq[SpaceProperties], val props: (String, Prop)*) extends SpireProperties {
    val bases = Seq("scalar" → sl(scalarLaws), "vector" → vl(vectorLaws))
  }

}

// vim: expandtab:ts=2:sw=2

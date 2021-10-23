/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.{Laws, Predicate}

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import scala.annotation.nowarn

import InvalidTestException._

object VectorSpaceLaws {
  def apply[V: Eq: Arbitrary, A: Eq: Arbitrary: Predicate] = new VectorSpaceLaws[V, A] {
    val scalarLaws = RingLaws[A]
    val vectorLaws = GroupLaws[V]
  }
}

trait VectorSpaceLaws[V, A] extends Laws with ModuleToRing[V, A] {
  implicit def cRingFromCModule(implicit V: CModule[V, A]): CRing[A] = V.scalar

  val scalarLaws: RingLaws[A]
  val vectorLaws: GroupLaws[V]

  import scalarLaws.{Arb => ArA, Equ => EqA}
  import vectorLaws.{Arb => ArV, Equ => EqV}

  def leftModule(implicit V: LeftModule[V, A]) = new SpaceProperties(
    name = "leftModule",
    sl = _.ring(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq.empty,
    "left scalar distributes" -> forAllSafe((r: A, v: V, w: V) => r *: (v + w) === (r *: v) + (r *: w)),
    "left vector distributes" -> forAllSafe((r: A, s: A, v: V) => (r + s) *: v === (r *: v) + (s *: v)),
    "left associative scalar" -> forAllSafe((r: A, s: A, v: V) => (r * s) *: v === r *: (s *: v)),
    "left identity" -> forAllSafe((v: V) => V.scalar.one *: v === v)
  )

  def rightModule(implicit V: RightModule[V, A]) = new SpaceProperties(
    name = "rightModule",
    sl = _.ring(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq.empty,
    "right scalar distributes" -> forAllSafe((v: V, w: V, r: A) => (v + w) :* r === (v :* r) + (w :* r)),
    "right vector distributes" -> forAllSafe((v: V, r: A, s: A) => v :* (r + s) === (v :* r) + (v :* s)),
    "right associative scalar" -> forAllSafe((v: V, r: A, s: A) => v :* (r * s) === (v :* r) :* s),
    "right identity" -> forAllSafe((v: V) => v :* V.scalar.one === v)
  )

  def cModule(implicit V: CModule[V, A]): SpaceProperties = new SpaceProperties(
    name = "cModule",
    sl = _.cRing(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(leftModule, rightModule),
    "left and right multiplication are compatible" -> forAllSafe((r: A, v: V, s: A) => r *: (v :* s) === (r *: v) :* s)
  )

  def vectorSpace(implicit V: VectorSpace[V, A]): SpaceProperties = new SpaceProperties(
    name = "vector space",
    sl = _.field(V.scalar),
    vl = _.abGroup(V.additive),
    parents = Seq(cModule)
  )

  def metricSpace(implicit V: MetricSpace[V, A], o: Order[A], A: AdditiveMonoid[A]): SpaceProperties =
    new SpaceProperties(
      name = "metric space",
      sl = _.emptyRuleSet,
      vl = _.emptyRuleSet,
      parents = Seq.empty,
      "identity" -> forAllSafe((x: V, y: V) =>
        if (x === y) V.distance(x, y) === A.zero
        else V.distance(x, y) =!= A.zero
      ),
      "symmetric" -> forAllSafe((x: V, y: V) => V.distance(x, y) === V.distance(y, x)),
      "triangle inequality" -> forAllSafe((x: V, y: V, z: V) =>
        V.distance(x, z) <= (V.distance(x, y) + V.distance(y, z))
      )
    )

  @nowarn
  def normedVectorSpace(implicit V: NormedVectorSpace[V, A], ev0: Order[A], ev1: Signed[A]): SpaceProperties =
    new SpaceProperties(
      name = "normed vector space",
      sl = _.field(V.scalar),
      vl = _.abGroup(V.additive),
      parents = Seq(vectorSpace, metricSpace),
      "scalable" -> forAllSafe((a: A, v: V) => a.abs * v.norm === (a.abs *: v).norm),
      "only 1 zero" -> forAllSafe((v: V) => // This is covered by metricSpace...
        if (v === V.zero)
          v.norm === Rng[A].zero
        else
          v.norm > Rng[A].zero
      )
    )

  def linearity(f: V => A)(implicit V: CModule[V, A]): SimpleRuleSet = new SimpleRuleSet(
    name = "linearity",
    "homogeneity" -> forAllSafe((r: A, v: V) => f(r *: v) === r * f(v)),
    "additivity" -> forAllSafe((v: V, w: V) => f(v + w) === f(v) + f(w))
  )

  @nowarn
  def innerProductSpace(implicit V: InnerProductSpace[V, A], A: Order[A], A0: Signed[A]): SpaceProperties =
    SpaceProperties.fromParent(
      name = "inner-product space",
      parent = vectorSpace,
      "symmetry" -> forAllSafe((v: V, w: V) => (v ⋅ w).abs === (w ⋅ v).abs),
      "linearity of partial inner product" -> forAllSafe((w: V) =>
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
    val bases = Seq("scalar" -> sl(scalarLaws), "vector" -> vl(vectorLaws))
  }

}

// vim: expandtab:ts=2:sw=2

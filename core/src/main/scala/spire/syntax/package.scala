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

package object syntax {
  @deprecated("Replaced by fastFor, *please* read fastFor scaladocs for details", "0.18.0")
  object cfor extends CforSyntax
  object fastFor extends FastForSyntax
  object literals extends LiteralsSyntax

  object eq extends EqSyntax
  object partialOrder extends PartialOrderSyntax
  object order extends OrderSyntax
  object signed extends SignedSyntax
  object truncatedDivision extends TruncatedDivisionSyntax

  object involution extends InvolutionSyntax
  object isReal extends IsRealSyntax
  object convertableFrom extends ConvertableFromSyntax

  object semigroupoid extends SemigroupoidSyntax
  object groupoid extends GroupoidSyntax

  object semigroup extends SemigroupSyntax
  object monoid extends MonoidSyntax
  object group extends GroupSyntax

  object additiveSemigroup extends AdditiveSemigroupSyntax
  object additiveMonoid extends AdditiveMonoidSyntax
  object additiveGroup extends AdditiveGroupSyntax

  object multiplicativeSemigroup extends MultiplicativeSemigroupSyntax
  object multiplicativeMonoid extends MultiplicativeMonoidSyntax
  object multiplicativeGroup extends MultiplicativeGroupSyntax

  object semiring extends SemiringSyntax
  object rig extends RigSyntax
  object rng extends RngSyntax
  object ring extends RingSyntax
  object gcdRing extends GCDRingSyntax
  object euclideanRing extends EuclideanRingSyntax
  object field extends FieldSyntax
  object nroot extends NRootSyntax
  object trig extends TrigSyntax

  object leftModule extends LeftModuleSyntax
  object rightModule extends RightModuleSyntax
  object cModule extends CModuleSyntax
  object vectorSpace extends VectorSpaceSyntax
  object metricSpace extends MetricSpaceSyntax
  object normedVectorSpace extends NormedVectorSpaceSyntax
  object innerProductSpace extends InnerProductSpaceSyntax
  object coordinateSpace extends CoordinateSpaceSyntax

  object lattice extends LatticeSyntax
  object heyting extends HeytingSyntax
  object logic extends LogicSyntax
  object bool extends BoolSyntax

  object bitString extends BitStringSyntax

  object partialAction extends PartialActionSyntax
  object action extends ActionSyntax
  object torsor extends TorsorSyntax

  object integral extends IntegralSyntax
  object fractional extends FractionalSyntax
  object numeric extends NumericSyntax

  object all extends AllSyntax

  @deprecated("Unbound syntax will be removed", "spire 0.18.0")
  object unbound extends UnboundSyntax

  object interval extends IntervalSyntax
}

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
package syntax

import spire.NotGiven
import spire.algebra._
import spire.algebra.lattice._
import spire.algebra.partial._
import spire.math._
import spire.macros.Syntax
import spire.syntax.std._
import scala.annotation.nowarn

trait EqSyntax {
  implicit def eqOps[A: Eq](a: A): EqOps[A] = new EqOps(a)
}

trait PartialOrderSyntax extends EqSyntax {
  implicit def partialOrderOps[A: PartialOrder](a: A): PartialOrderOps[A] = new PartialOrderOps(a)
}

trait OrderSyntax extends PartialOrderSyntax {
  implicit def orderOps[A: Order](a: A): OrderOps[A] = new OrderOps(a)
  implicit def literalIntOrderOps(lhs: Int): LiteralIntOrderOps = new LiteralIntOrderOps(lhs)
  implicit def literalLongOrderOps(lhs: Long): LiteralLongOrderOps = new LiteralLongOrderOps(lhs)
  implicit def literalDoubleOrderOps(lhs: Double): LiteralDoubleOrderOps = new LiteralDoubleOrderOps(lhs)
}

trait SignedSyntax extends OrderSyntax {
  implicit def signedOps[A: Signed](a: A): SignedOps[A] = new SignedOps(a)
}

trait TruncatedDivisionSyntax extends SignedSyntax {
  implicit def truncatedDivisionOps[A: TruncatedDivision](a: A): TruncatedDivisionOps[A] = new TruncatedDivisionOps(a)
  implicit def literalIntTruncatedDivisionOps(lhs: Int): LiteralIntTruncatedDivisionOps =
    new LiteralIntTruncatedDivisionOps(lhs)
  implicit def literalLongTruncatedDivisionOps(lhs: Long): LiteralLongTruncatedDivisionOps =
    new LiteralLongTruncatedDivisionOps(lhs)
  implicit def literalDoubleTruncatedDivisionOps(lhs: Double): LiteralDoubleTruncatedDivisionOps =
    new LiteralDoubleTruncatedDivisionOps(lhs)
}

trait InvolutionSyntax {
  implicit def involutionOps[A: Involution](lhs: A): InvolutionOps[A] = new InvolutionOps(lhs)
}

trait IsRealSyntax extends SignedSyntax {
  implicit def isRealOps[A: IsReal](a: A): IsRealOps[A] = new IsRealOps(a)
}

trait SemigroupoidSyntax {
  implicit def semigroupoidOps[A: Semigroupoid](a: A): SemigroupoidOps[A] = new SemigroupoidOps[A](a)
}

trait GroupoidSyntax extends SemigroupoidSyntax {
  @nowarn
  implicit def groupoidCommonOps[A](a: A)(implicit ev: Groupoid[A], ni: NotGiven[Monoid[A]]): GroupoidCommonOps[A] =
    new GroupoidCommonOps[A](a)(ev)
  implicit def groupoidOps[A](a: A)(implicit ev: Groupoid[A]): GroupoidOps[A] = new GroupoidOps[A](a)
}

trait SemigroupSyntax {
  implicit def semigroupOps[A: Semigroup](a: A): SemigroupOps[A] = new SemigroupOps(a)
}

trait MonoidSyntax extends SemigroupSyntax {
  implicit def monoidOps[A](a: A)(implicit ev: Monoid[A]): MonoidOps[A] = new MonoidOps(a)
}

trait GroupSyntax extends MonoidSyntax {
  implicit def groupOps[A: Group](a: A): GroupOps[A] = new GroupOps(a)
}

trait AdditiveSemigroupSyntax {
  implicit def additiveSemigroupOps[A: AdditiveSemigroup](a: A): AdditiveSemigroupOps[A] = new AdditiveSemigroupOps(a)
  implicit def literalIntAdditiveSemigroupOps(lhs: Int): LiteralIntAdditiveSemigroupOps =
    new LiteralIntAdditiveSemigroupOps(lhs)
  implicit def literalLongAdditiveSemigroupOps(lhs: Long): LiteralLongAdditiveSemigroupOps =
    new LiteralLongAdditiveSemigroupOps(lhs)
  implicit def literalDoubleAdditiveSemigroupOps(lhs: Double): LiteralDoubleAdditiveSemigroupOps =
    new LiteralDoubleAdditiveSemigroupOps(lhs)
}

trait AdditiveMonoidSyntax extends AdditiveSemigroupSyntax {
  implicit def additiveMonoidOps[A](a: A)(implicit ev: AdditiveMonoid[A]): AdditiveMonoidOps[A] = new AdditiveMonoidOps(
    a
  )
}

trait AdditiveGroupSyntax extends AdditiveMonoidSyntax {
  implicit def additiveGroupOps[A: AdditiveGroup](a: A): AdditiveGroupOps[A] = new AdditiveGroupOps(a)
  implicit def literalIntAdditiveGroupOps(lhs: Int): LiteralIntAdditiveGroupOps = new LiteralIntAdditiveGroupOps(lhs)
  implicit def literalLongAdditiveGroupOps(lhs: Long): LiteralLongAdditiveGroupOps = new LiteralLongAdditiveGroupOps(
    lhs
  )
  implicit def literalDoubleAdditiveGroupOps(lhs: Double): LiteralDoubleAdditiveGroupOps =
    new LiteralDoubleAdditiveGroupOps(lhs)
}

trait MultiplicativeSemigroupSyntax {
  implicit def multiplicativeSemigroupOps[A: MultiplicativeSemigroup](a: A): MultiplicativeSemigroupOps[A] =
    new MultiplicativeSemigroupOps(a)
  implicit def literalIntMultiplicativeSemigroupOps(lhs: Int): LiteralIntMultiplicativeSemigroupOps =
    new LiteralIntMultiplicativeSemigroupOps(lhs)
  implicit def literalLongMultiplicativeSemigroupOps(lhs: Long): LiteralLongMultiplicativeSemigroupOps =
    new LiteralLongMultiplicativeSemigroupOps(lhs)
  implicit def literalDoubleMultiplicativeSemigroupOps(lhs: Double): LiteralDoubleMultiplicativeSemigroupOps =
    new LiteralDoubleMultiplicativeSemigroupOps(lhs)
}

trait MultiplicativeMonoidSyntax extends MultiplicativeSemigroupSyntax {
  implicit def multiplicativeMonoidOps[A](a: A)(implicit ev: MultiplicativeMonoid[A]): MultiplicativeMonoidOps[A] =
    new MultiplicativeMonoidOps(a)
}

trait MultiplicativeGroupSyntax extends MultiplicativeMonoidSyntax {
  implicit def multiplicativeGroupOps[A: MultiplicativeGroup](a: A): MultiplicativeGroupOps[A] =
    new MultiplicativeGroupOps(a)
  implicit def literalIntMultiplicativeGroupOps(lhs: Int): LiteralIntMultiplicativeGroupOps =
    new LiteralIntMultiplicativeGroupOps(lhs)
  implicit def literalLongMultiplicativeGroupOps(lhs: Long): LiteralLongMultiplicativeGroupOps =
    new LiteralLongMultiplicativeGroupOps(lhs)
  implicit def literalDoubleMultiplicativeGroupOps(lhs: Double): LiteralDoubleMultiplicativeGroupOps =
    new LiteralDoubleMultiplicativeGroupOps(lhs)
}

trait SemiringSyntax extends AdditiveSemigroupSyntax with MultiplicativeSemigroupSyntax {
  implicit def semiringOps[A: Semiring](a: A): SemiringOps[A] = new SemiringOps(a)
}

trait RigSyntax extends SemiringSyntax

trait RngSyntax extends SemiringSyntax with AdditiveGroupSyntax

trait RingSyntax extends RngSyntax with RigSyntax

trait GCDRingSyntax extends RingSyntax {
  implicit def gcdRingOps[A: GCDRing](a: A): GCDRingOps[A] = new GCDRingOps(a)
}

trait EuclideanRingSyntax extends GCDRingSyntax {
  implicit def euclideanRingOps[A: EuclideanRing](a: A): EuclideanRingOps[A] = new EuclideanRingOps(a)
  implicit def literalIntEuclideanRingOps(lhs: Int): LiteralIntEuclideanRingOps = new LiteralIntEuclideanRingOps(lhs)
  implicit def literalLongEuclideanRingOps(lhs: Long): LiteralLongEuclideanRingOps = new LiteralLongEuclideanRingOps(
    lhs
  )
  implicit def literalDoubleEuclideanRingOps(lhs: Double): LiteralDoubleEuclideanRingOps =
    new LiteralDoubleEuclideanRingOps(lhs)
}

trait FieldSyntax extends EuclideanRingSyntax with MultiplicativeGroupSyntax

trait NRootSyntax {
  implicit def nrootOps[A: NRoot](a: A): NRootOps[A] = new NRootOps(a)
}

trait LeftModuleSyntax extends RingSyntax {
  implicit def leftModuleOps[V](v: V): LeftModuleOps[V] = new LeftModuleOps[V](v)
}

trait RightModuleSyntax extends RingSyntax {
  implicit def rightModuleOps[V](v: V): RightModuleOps[V] = new RightModuleOps[V](v)
}

trait CModuleSyntax extends LeftModuleSyntax with RightModuleSyntax

trait VectorSpaceSyntax extends CModuleSyntax with FieldSyntax {
  implicit def vectorSpaceOps[V](v: V): VectorSpaceOps[V] = new VectorSpaceOps[V](v)
}

trait MetricSpaceSyntax extends VectorSpaceSyntax {
  implicit def metricSpaceOps[V](v: V): MetricSpaceOps[V] = new MetricSpaceOps[V](v)
}

trait NormedVectorSpaceSyntax extends MetricSpaceSyntax {
  implicit def normedVectorSpaceOps[V](v: V): NormedVectorSpaceOps[V] = new NormedVectorSpaceOps[V](v)
}

trait InnerProductSpaceSyntax extends VectorSpaceSyntax {
  implicit def innerProductSpaceOps[V](v: V): InnerProductSpaceOps[V] = new InnerProductSpaceOps[V](v)
}

trait CoordinateSpaceSyntax extends InnerProductSpaceSyntax {
  implicit def coordinateSpaceOps[V](v: V): CoordinateSpaceOps[V] = new CoordinateSpaceOps[V](v)
}

trait TrigSyntax {
  implicit def trigOps[A: Trig](a: A): TrigOps[A] = new TrigOps(a)
}

trait LatticeSyntax {
  implicit def meetOps[A: MeetSemilattice](a: A): MeetOps[A] = new MeetOps(a)
  implicit def joinOps[A: JoinSemilattice](a: A): JoinOps[A] = new JoinOps(a)
}

trait HeytingSyntax {
  implicit def heytingOps[A: Heyting](a: A): HeytingOps[A] = new HeytingOps(a)
}

trait LogicSyntax {
  implicit def logicOps[A: Logic](a: A): LogicOps[A] = new LogicOps(a)
}

trait BoolSyntax extends HeytingSyntax {
  implicit def boolOps[A: Bool](a: A): BoolOps[A] = new BoolOps(a)
}

trait BitStringSyntax {
  implicit def bitStringOps[A: BitString](a: A): BitStringOps[A] = new BitStringOps(a)
}

trait PartialActionSyntax {
  implicit def leftPartialActionOps[G](g: G): LeftPartialActionOps[G] = new LeftPartialActionOps(g)
  implicit def rightPartialActionOps[P](p: P): RightPartialActionOps[P] = new RightPartialActionOps(p)
}

trait ActionSyntax {
  implicit def leftActionOps[G](g: G): LeftActionOps[G] = new LeftActionOps(g)
  implicit def rightActionOps[P](p: P): RightActionOps[P] = new RightActionOps(p)
}

trait IntervalSyntax {
  implicit def intervalOps[A: Order: AdditiveGroup](a: A): IntervalPointOps[A] =
    new IntervalPointOps(a)
}

@deprecated("Unbound syntax will be removed", "spire 0.18.0")
trait UnboundSyntax {
  implicit def moduleUnboundOps[F](f: F)(implicit ev: CModule[?, F]): ModuleUnboundOps[F] =
    new ModuleUnboundOps(f)

  implicit def vectorSpaceUnboundOps[F](f: F)(implicit ev: VectorSpace[?, F]): VectorSpaceUnboundOps[F] =
    new VectorSpaceUnboundOps(f)

  implicit def groupActionUnboundOps[G](g: G)(implicit ev: Action[?, G]): ActionUnboundOps[G] =
    new ActionUnboundOps(g)
  implicit def additiveActionUnboundOps[G](g: G)(implicit ev: AdditiveAction[?, G]): AdditiveActionUnboundOps[G] =
    new AdditiveActionUnboundOps(g)
  implicit def multiplicativeActionUnboundOps[G](g: G)(implicit
    ev: MultiplicativeAction[?, G]
  ): MultiplicativeActionUnboundOps[G] =
    new MultiplicativeActionUnboundOps(g)
}

trait TorsorSyntax {
  implicit def torsorPointOps[P](p: P): TorsorPointOps[P] = new TorsorPointOps(p)
}

trait IntegralSyntax extends EuclideanRingSyntax with ConvertableFromSyntax with OrderSyntax with SignedSyntax {
  implicit def integralOps[A: Integral](a: A): IntegralOps[A] = new IntegralOps(a)
}

trait FractionalSyntax
    extends FieldSyntax
    with NRootSyntax
    with ConvertableFromSyntax
    with OrderSyntax
    with SignedSyntax

trait NumericSyntax extends FieldSyntax with NRootSyntax with ConvertableFromSyntax with OrderSyntax with SignedSyntax

trait ConvertableFromSyntax {
  implicit def convertableOps[A: ConvertableFrom](a: A): ConvertableFromOps[A] = new ConvertableFromOps(a)
}

trait CforSyntax {
  def cfor[A](init: A)(test: A => Boolean, next: A => A)(body: A => Unit): Unit =
    macro Syntax.cforMacro[A]
  def cforRange(r: Range)(body: Int => Unit): Unit =
    macro Syntax.cforRangeMacro
  def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit =
    macro Syntax.cforRange2Macro
}

trait LiteralsSyntax {
  implicit def literals(s: StringContext): Literals = new Literals(s)

  object radix { implicit def radix(s: StringContext): Radix = new Radix(s) }
  object si { implicit def siLiterals(s: StringContext): SiLiterals = new SiLiterals(s) }
  object us { implicit def usLiterals(s: StringContext): UsLiterals = new UsLiterals(s) }
  object eu { implicit def euLiterals(s: StringContext): EuLiterals = new EuLiterals(s) }
}

trait AllButCatsSyntax
    extends LiteralsSyntax
    with CforSyntax
    with SignedSyntax
    with TruncatedDivisionSyntax
    with InvolutionSyntax
    with IsRealSyntax
    with ConvertableFromSyntax
    with SemigroupoidSyntax
    with GroupoidSyntax
    with AdditiveSemigroupSyntax
    with AdditiveMonoidSyntax
    with AdditiveGroupSyntax
    with MultiplicativeSemigroupSyntax
    with MultiplicativeMonoidSyntax
    with MultiplicativeGroupSyntax
    with SemiringSyntax
    with RigSyntax
    with RngSyntax
    with RingSyntax
    with GCDRingSyntax
    with EuclideanRingSyntax
    with FieldSyntax
    with NRootSyntax
    with TrigSyntax
    with IntervalSyntax
    with LeftModuleSyntax
    with RightModuleSyntax
    with CModuleSyntax
    with VectorSpaceSyntax
    with NormedVectorSpaceSyntax
    with InnerProductSpaceSyntax
    with CoordinateSpaceSyntax
    with LatticeSyntax
    with LogicSyntax
    with HeytingSyntax
    with BoolSyntax
    with BitStringSyntax
    with PartialActionSyntax
    with ActionSyntax
    with TorsorSyntax
    with IntegralSyntax
    with FractionalSyntax
    with NumericSyntax
    with IntSyntax
    with LongSyntax
    with DoubleSyntax
    with BigIntSyntax
    with ArraySyntax
    with SeqSyntax

trait AllSyntax
    extends AllButCatsSyntax
    with EqSyntax
    with PartialOrderSyntax
    with OrderSyntax
    with SemigroupSyntax
    with MonoidSyntax
    with GroupSyntax

package spire.syntax

import spire.algebra._
import spire.algebra.lattice._
import spire.math._
import spire.macros.Syntax
import spire.syntax.std._

trait EqSyntax {
  implicit def eqOps[A:Eq](a:A) = new EqOps(a)
}

trait PartialOrderSyntax extends EqSyntax {
  implicit def partialOrderOps[A:PartialOrder](a:A) = new PartialOrderOps(a)
}

trait OrderSyntax extends PartialOrderSyntax {
  implicit def orderOps[A:Order](a:A) = new OrderOps(a)
  implicit def literalIntOrderOps(lhs: Int) = new LiteralIntOrderOps(lhs)
  implicit def literalLongOrderOps(lhs: Long) = new LiteralLongOrderOps(lhs)
  implicit def literalDoubleOrderOps(lhs: Double) = new LiteralDoubleOrderOps(lhs)
}

trait IsRealSyntax extends OrderSyntax with SignedSyntax {
  implicit def isRealOps[A:IsReal](a:A) = new IsRealOps(a)
}

trait SignedSyntax {
  implicit def signedOps[A: Signed](a: A) = new SignedOps(a)
}

trait SemigroupSyntax {
  implicit def semigroupOps[A:Semigroup](a:A) = new SemigroupOps(a)
}

trait MonoidSyntax extends SemigroupSyntax {
  implicit def monoidOps[A](a:A)(implicit ev: Monoid[A]) = new MonoidOps(a)
}

trait GroupSyntax extends MonoidSyntax {
  implicit def groupOps[A:Group](a:A) = new GroupOps(a)
}

trait AdditiveSemigroupSyntax {
  implicit def additiveSemigroupOps[A:AdditiveSemigroup](a:A) = new AdditiveSemigroupOps(a)
  implicit def literalIntAdditiveSemigroupOps(lhs:Int) = new LiteralIntAdditiveSemigroupOps(lhs)
  implicit def literalLongAdditiveSemigroupOps(lhs:Long) = new LiteralLongAdditiveSemigroupOps(lhs)
  implicit def literalDoubleAdditiveSemigroupOps(lhs:Double) = new LiteralDoubleAdditiveSemigroupOps(lhs)
}

trait AdditiveMonoidSyntax extends AdditiveSemigroupSyntax {
  implicit def additiveMonoidOps[A](a:A)(implicit ev: AdditiveMonoid[A]) = new AdditiveMonoidOps(a)
}

trait AdditiveGroupSyntax extends AdditiveMonoidSyntax {
  implicit def additiveGroupOps[A:AdditiveGroup](a:A) = new AdditiveGroupOps(a)
  implicit def literalIntAdditiveGroupOps(lhs:Int) = new LiteralIntAdditiveGroupOps(lhs)
  implicit def literalLongAdditiveGroupOps(lhs:Long) = new LiteralLongAdditiveGroupOps(lhs)
  implicit def literalDoubleAdditiveGroupOps(lhs:Double) = new LiteralDoubleAdditiveGroupOps(lhs)
}

trait MultiplicativeSemigroupSyntax {
  implicit def multiplicativeSemigroupOps[A:MultiplicativeSemigroup](a:A) = new MultiplicativeSemigroupOps(a)
  implicit def literalIntMultiplicativeSemigroupOps(lhs:Int) = new LiteralIntMultiplicativeSemigroupOps(lhs)
  implicit def literalLongMultiplicativeSemigroupOps(lhs:Long) = new LiteralLongMultiplicativeSemigroupOps(lhs)
  implicit def literalDoubleMultiplicativeSemigroupOps(lhs:Double) = new LiteralDoubleMultiplicativeSemigroupOps(lhs)
}

trait MultiplicativeMonoidSyntax extends MultiplicativeSemigroupSyntax {
  implicit def multiplicativeMonoidOps[A](a:A)(implicit ev: MultiplicativeMonoid[A]) =
    new MultiplicativeMonoidOps(a)
}

trait MultiplicativeGroupSyntax extends MultiplicativeMonoidSyntax {
  implicit def multiplicativeGroupOps[A:MultiplicativeGroup](a:A) = new MultiplicativeGroupOps(a)
  implicit def literalIntMultiplicativeGroupOps(lhs:Int) = new LiteralIntMultiplicativeGroupOps(lhs)
  implicit def literalLongMultiplicativeGroupOps(lhs:Long) = new LiteralLongMultiplicativeGroupOps(lhs)
  implicit def literalDoubleMultiplicativeGroupOps(lhs:Double) = new LiteralDoubleMultiplicativeGroupOps(lhs)
}

trait SemiringSyntax extends AdditiveSemigroupSyntax with MultiplicativeSemigroupSyntax {
  implicit def semiringOps[A:Semiring](a:A) = new SemiringOps(a)
}

trait RigSyntax extends SemiringSyntax

trait RngSyntax extends SemiringSyntax with AdditiveGroupSyntax

trait RingSyntax extends RngSyntax with RigSyntax

trait EuclideanRingSyntax extends RingSyntax {
  implicit def euclideanRingOps[A:EuclideanRing](a:A) = new EuclideanRingOps(a)
  implicit def literalIntEuclideanRingOps(lhs:Int) = new LiteralIntEuclideanRingOps(lhs)
  implicit def literalLongEuclideanRingOps(lhs:Long) = new LiteralLongEuclideanRingOps(lhs)
  implicit def literalDoubleEuclideanRingOps(lhs:Double) = new LiteralDoubleEuclideanRingOps(lhs)
}

trait FieldSyntax extends EuclideanRingSyntax with MultiplicativeGroupSyntax

trait NRootSyntax {
  implicit def nrootOps[A: NRoot](a: A) = new NRootOps(a)
}

trait ModuleSyntax extends RingSyntax {
  implicit def moduleOps[V](v:V) = new ModuleOps[V](v)
}

trait VectorSpaceSyntax extends ModuleSyntax with FieldSyntax {
  implicit def vectorSpaceOps[V](v:V) = new VectorSpaceOps[V](v)
}

trait MetricSpaceSyntax extends VectorSpaceSyntax {
  implicit def metricSpaceOps[V](v:V) = new MetricSpaceOps[V](v)
}

trait NormedVectorSpaceSyntax extends MetricSpaceSyntax {
  implicit def normedVectorSpaceOps[V](v:V) = new NormedVectorSpaceOps[V](v)
}

trait InnerProductSpaceSyntax extends VectorSpaceSyntax {
  implicit def innerProductSpaceOps[V](v:V) = new InnerProductSpaceOps[V](v)
}

trait CoordinateSpaceSyntax extends InnerProductSpaceSyntax {
  implicit def coordianteSpaceOps[V](v:V) = new CoordinateSpaceOps[V](v)
}

trait TrigSyntax {
  implicit def trigOps[A:Trig](a: A) = new TrigOps(a)
}

trait LatticeSyntax {
  implicit def meetOps[A: MeetSemilattice](a: A) = new MeetOps(a)
  implicit def joinOps[A: JoinSemilattice](a: A) = new JoinOps(a)
}

trait HeytingSyntax {
  implicit def heytingOps[A: Heyting](a: A) = new HeytingOps(a)
}

trait BoolSyntax extends HeytingSyntax {
  implicit def boolOps[A: Bool](a: A) = new BoolOps(a)
}

trait BitStringSyntax {
  implicit def bitStringOps[A: BitString](a: A) = new BitStringOps(a)
}

trait ActionSyntax {
  implicit def leftActionOps[G](g: G) = new LeftActionOps(g)
  implicit def rightActionOps[P](p: P) = new RightActionOps(p)
}

trait UnboundSyntax {
  implicit def moduleUnboundOps[F](f: F)(implicit ev: Module[_, F]) =
    new ModuleUnboundOps(f)

  implicit def vectorSpaceUnboundOps[F](f: F)(implicit ev: VectorSpace[_, F]) =
    new VectorSpaceUnboundOps(f)

  implicit def groupActionUnboundOps[G](g: G)(implicit ev: Action[_, G]) =
    new ActionUnboundOps(g)
  implicit def additiveActionUnboundOps[G](g: G)(implicit ev: AdditiveAction[_, G]) =
    new AdditiveActionUnboundOps(g)
  implicit def multiplicativeActionUnboundOps[G](g: G)(implicit ev: MultiplicativeAction[_, G]) =
    new MultiplicativeActionUnboundOps(g)
}

trait TorsorSyntax {
  implicit def torsorPointOps[P](p: P) = new TorsorPointOps(p)
}

trait IntegralSyntax extends
    EuclideanRingSyntax with
    ConvertableFromSyntax with
    OrderSyntax with
    SignedSyntax {
  implicit def integralOps[A: Integral](a: A) = new IntegralOps(a)
}

trait FractionalSyntax extends
    FieldSyntax with
    NRootSyntax with
    ConvertableFromSyntax with
    OrderSyntax with
    SignedSyntax

trait NumericSyntax extends
    FieldSyntax with
    NRootSyntax with
    ConvertableFromSyntax with
    OrderSyntax with
    SignedSyntax

trait ConvertableFromSyntax {
  implicit def convertableOps[A:ConvertableFrom](a:A) = new ConvertableFromOps(a)
}

trait CforSyntax {
  def cfor[A](init:A)(test:A => Boolean, next:A => A)(body:A => Unit): Unit =
    macro Syntax.cforMacro[A]
  def cforRange(r: Range)(body: Int => Unit): Unit =
    macro Syntax.cforRangeMacro
  def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit =
    macro Syntax.cforRange2Macro
}

trait LiteralsSyntax {
  implicit def literals(s:StringContext) = new Literals(s)

  object radix { implicit def radix(s:StringContext) = new Radix(s) }
  object si { implicit def siLiterals(s:StringContext) = new SiLiterals(s) }
  object us { implicit def usLiterals(s:StringContext) = new UsLiterals(s) }
  object eu { implicit def euLiterals(s:StringContext) = new EuLiterals(s) }
}

trait AllSyntax extends
    LiteralsSyntax with
    CforSyntax with
    EqSyntax with
    PartialOrderSyntax with
    OrderSyntax with
    SignedSyntax with
    IsRealSyntax with
    ConvertableFromSyntax with
    SemigroupSyntax with
    MonoidSyntax with
    GroupSyntax with
    AdditiveSemigroupSyntax with
    AdditiveMonoidSyntax with
    AdditiveGroupSyntax with
    MultiplicativeSemigroupSyntax with
    MultiplicativeMonoidSyntax with
    MultiplicativeGroupSyntax with
    SemiringSyntax with
    RigSyntax with
    RngSyntax with
    RingSyntax with
    EuclideanRingSyntax with
    FieldSyntax with
    NRootSyntax with
    TrigSyntax with
    ModuleSyntax with
    VectorSpaceSyntax with
    NormedVectorSpaceSyntax with
    InnerProductSpaceSyntax with
    CoordinateSpaceSyntax with
    LatticeSyntax with
    HeytingSyntax with
    BoolSyntax with
    BitStringSyntax with
    ActionSyntax with
    TorsorSyntax with
    IntegralSyntax with
    FractionalSyntax with
    NumericSyntax with
    IntSyntax with
    LongSyntax with
    DoubleSyntax with
    BigIntSyntax with
    ArraySyntax with
    SeqSyntax

package spire.syntax

import spire.algebra._
import spire.math._
import spire.macros.Syntax
import spire.macrosk._
import spire.syntax.std._

trait EqSyntax {
  implicit def eqOps[A:Eq](a:A) = new EqOps(a)
}

trait OrderSyntax extends EqSyntax {
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

trait MonoidSyntax extends SemigroupSyntax

trait GroupSyntax extends MonoidSyntax {
  implicit def groupOps[A:Group](a:A) = new GroupOps(a)
}

trait AdditiveSemigroupSyntax {
  implicit def additiveSemigroupOps[A:AdditiveSemigroup](a:A) = new AdditiveSemigroupOps(a)
  implicit def literalIntAdditiveSemigroupOps(lhs:Int) = new LiteralIntAdditiveSemigroupOps(lhs)
  implicit def literalLongAdditiveSemigroupOps(lhs:Long) = new LiteralLongAdditiveSemigroupOps(lhs)
  implicit def literalDoubleAdditiveSemigroupOps(lhs:Double) = new LiteralDoubleAdditiveSemigroupOps(lhs)
}

trait AdditiveMonoidSyntax extends AdditiveSemigroupSyntax

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

trait MultiplicativeMonoidSyntax extends MultiplicativeSemigroupSyntax

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

trait BooleanAlgebraSyntax {
  implicit def booleanAlgebraOps[A:BooleanAlgebra](a: A) = new BooleanAlgebraOps(a)
}

trait BitStringSyntax {
  implicit def bitStringOps[A: BitString](a: A) = new BitStringOps(a)
}

trait AdditiveGroupActionSyntax {
  implicit def additiveGroupActionGroupOps[G](g: G) = new AdditiveGroupActionGroupOps(g)
  implicit def additiveGroupActionPointOps[P](p: P) = new AdditiveGroupActionPointOps(p)
}

trait MultiplicativeGroupActionSyntax {
  implicit def multiplicativeGroupActionGroupOps[G](g: G) = new MultiplicativeGroupActionGroupOps(g)
  implicit def multiplicativeGroupActionPointOps[P](p: P) = new MultiplicativeGroupActionPointOps(p)
}

trait AdditiveTorsorSyntax {
  implicit def additiveTorsorPointOps[P](p: P) = new AdditiveTorsorPointOps[P](p)
}

trait MultiplicativeTorsorSyntax {
  implicit def multiplicativeTorsorPointOps[P](p: P) = new MultiplicativeTorsorPointOps[P](p)
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
    BooleanAlgebraSyntax with
    BitStringSyntax with
    AdditiveGroupActionSyntax with
    MultiplicativeGroupActionSyntax with
    AdditiveTorsorSyntax with
    MultiplicativeTorsorSyntax with
    IntegralSyntax with
    FractionalSyntax with
    NumericSyntax with
    IntSyntax with
    LongSyntax with
    DoubleSyntax with
    BigIntSyntax with
    ArraySyntax with
    SeqSyntax

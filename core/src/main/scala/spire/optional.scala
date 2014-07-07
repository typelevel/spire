package spire.optional

import java.math.BigInteger
import java.lang.Long.numberOfTrailingZeros
import java.lang.Math

import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.reflect.ClassTag
import scala.{specialized => spec}

import spire.algebra._
import spire.math.Rational
import spire.std.{ SeqVectorEq, SeqVectorOrder }
import spire.std.{ ArrayVectorEq, ArrayVectorOrder }
import spire.std.MapVectorEq

/**
 * This provides an implicit `Eq[A]` for any type `A` using Scala's (Java's)
 * `==` (`equals`). This is generally considered a bad idea, since it means you
 * lose all type safety -- for instance, any 2 types can always be compared as
 * `Eq[Any]`.
 */
object genericEq {
  @SerialVersionUID(0L)
  private class GenericEq[@spec A] extends Eq[A] with Serializable {
    def eqv(x:A, y:A): Boolean = x == y
  }

  implicit def generic[@spec A]: Eq[A] = new GenericEq[A]
}

trait VectorOrderLow {
  implicit def seqEq[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      A0: Eq[A], module: Module[CC[A], A]) = new SeqVectorEq[A, CC[A]]()(A0, module.scalar)

  implicit def arrayEq[@spec(Int,Long,Float,Double) A](implicit ev: Eq[A], module: Module[Array[A], A]) =
    new ArrayVectorEq[A]()(ev, module.scalar)

  implicit def mapEq[K, V](implicit V0: Eq[V], module: Module[Map[K, V], V]) =
    new MapVectorEq[K, V]()(V0, module.scalar)
}

/**
 * This object provides implicit instances of Eq and Order for Seq-likes
 * that will behave like infinite vectors. Essentially all this means is that
 * `Seq(0, 0, 0) === Seq()`, since both are infinite vectors of zeros. Any
 * element not explicitly set is implied to be 0.
 */
object vectorOrder extends VectorOrderLow {
  implicit def seqOrder[A, CC[A] <: SeqLike[A, CC[A]]](implicit
      A0: Order[A], module: Module[CC[A], A]) = new SeqVectorOrder[A, CC[A]]()(A0, module.scalar)

  implicit def arrayOrder[@spec(Int,Long,Float,Double) A](implicit ev: Order[A], module: Module[Array[A], A]) =
    new ArrayVectorOrder[A]()(ev, module.scalar)
}

/**
 * This provides orderings (Order and Eq) for Float and Double that have
 * a total order. Specifically, this will order NaN's consistently, rather
 * than having their order be undefined. However, this won't be as fast as
 * the default ordering.
 */
object totalfloat {
  trait TotalFloatOrder extends Order[Float] {
    override def eqv(x:Float, y:Float) = java.lang.Float.compare(x, y) == 0
    override def neqv(x:Float, y:Float) = java.lang.Float.compare(x, y) != 0
    override def gt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
    override def gteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
    override def lt(x: Float, y: Float) = java.lang.Float.compare(x, y) > 0
    override def lteqv(x: Float, y: Float) = java.lang.Float.compare(x, y) >= 0
    override def min(x: Float, y: Float) = if (java.lang.Float.compare(x, y) < 0) x else y
    override def max(x: Float, y: Float) = Math.max(x, y)
    def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
  }
  implicit final val TotalFloatOrder = new TotalFloatOrder {}

  trait TotalDoubleOrder extends Order[Double] {
    override def eqv(x:Double, y:Double) = java.lang.Double.compare(x, y) == 0
    override def neqv(x:Double, y:Double) = java.lang.Double.compare(x, y) != 0
    override def gt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
    override def gteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
    override def lt(x: Double, y: Double) = java.lang.Double.compare(x, y) > 0
    override def lteqv(x: Double, y: Double) = java.lang.Double.compare(x, y) >= 0
    override def min(x: Double, y: Double) = if (java.lang.Double.compare(x, y) < 0) x else y
    override def max(x: Double, y: Double) = Math.max(x, y)
    def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
  }
  implicit final val TotalDoubleOrder = new TotalDoubleOrder {}
}

object rationalTrig {
  implicit val trigRational = new Trig[Rational] {
    val r180 = Rational(180)
    import spire.std.double._
    def acos(a: Rational): Rational = Rational(spire.math.acos(a.toDouble))
    def asin(a: Rational): Rational = Rational(spire.math.asin(a.toDouble))
    def atan(a: Rational): Rational = Rational(spire.math.atan(a.toDouble))
    def atan2(y: Rational,x: Rational): Rational = Rational(spire.math.atan2(y.toDouble, x.toDouble))
    def cos(a: Rational): Rational = Rational(spire.math.cos(a.toDouble))
    def cosh(x: Rational): Rational = Rational(spire.math.cosh(x.toDouble))
    val e: Rational = Rational(spire.math.e)
    def exp(a: Rational): Rational = Rational(spire.math.exp(a.toDouble))
    def expm1(a: Rational): Rational = Rational(spire.math.expm1(a.toDouble))
    def log(a: Rational): Rational = Rational(spire.math.log(a.toDouble))
    def log1p(a: Rational): Rational = Rational(spire.math.log1p(a.toDouble))
    val pi: Rational = Rational(spire.math.pi)
    def sin(a: Rational): Rational = Rational(spire.math.sin(a.toDouble))
    def sinh(x: Rational): Rational = Rational(spire.math.sinh(x.toDouble))
    def tan(a: Rational): Rational = Rational(spire.math.tan(a.toDouble))
    def tanh(x: Rational): Rational = Rational(spire.math.tanh(x.toDouble))
    def toDegrees(a: Rational): Rational = (a * r180) / pi
    def toRadians(a: Rational): Rational = (a / r180) * pi
  }
}

object powerSetPartialOrder {
  /** Set partial order defined as follows:
    * 
    * S <= T if S is a subset of T.
    */
  class PowerSetPartialOrder[A] extends PartialOrder[Set[A]] {
    override def eqv(x: Set[A], y: Set[A]) = (x == y)
    override def lteqv(x: Set[A], y: Set[A]) = x.subsetOf(y)
    override def lt(x: Set[A], y: Set[A]) = x.subsetOf(y) && x != y
    override def gteqv(x: Set[A], y: Set[A]) = y.subsetOf(x)
    override def gt(x: Set[A], y: Set[A]) = y.subsetOf(x) && x != y

    def partialCompare(x: Set[A], y: Set[A]): Double = {
      if (eqv(x, y))
        0.0
      else if (lt(x, y))
        -1.0
      else if (gt(x, y))
        1.0
      else
        Double.NaN
    }
  }
  implicit def powerSetPartialOrder[A]: PartialOrder[Set[A]] = new PowerSetPartialOrder[A]
}

object intervalSubsetPartialOrder {
  import spire.math.Interval
  import spire.implicits._

  /** Interval partial order defined as follows:
    * 
    * I <= J if I is a subset of J.
    */
  class IntervalSubsetPartialOrder[A: Order] extends PartialOrder[Interval[A]] {
    override def eqv(x: Interval[A], y: Interval[A]) = (x == y)
    override def lteqv(x: Interval[A], y: Interval[A]) = x.isSubsetOf(y)
    override def lt(x: Interval[A], y: Interval[A]) = x.isProperSubsetOf(y)
    override def gteqv(x: Interval[A], y: Interval[A]) = x.isSupersetOf(y)
    override def gt(x: Interval[A], y: Interval[A]) = x.isProperSupersetOf(y)

    def partialCompare(x: Interval[A], y: Interval[A]): Double = {
      if (eqv(x, y))
        0.0
      else if (lt(x, y))
        -1.0
      else if (gt(x, y))
        1.0
      else
        Double.NaN
    }
  }
  implicit def intervalSubsetPartialOrder[A: Order]: PartialOrder[Interval[A]] = new IntervalSubsetPartialOrder[A]
}

object intervalGeometricPartialOrder {
  import spire.math.Interval
  import spire.implicits._
  import Interval.{Open, Closed}
  /** Interval partial order defined as follows:
    *
    * Involving empty intervals:
    * 
    * - if I and J are empty, then I === J.
    * - if I (resp. J) is empty and J (resp. I) is non-empty,
    *   the ordering is undefined (preserving antisymmetry).
    * 
    * For non-empty intervals:
    * 
    * - I === J is standard Eq semantics (I, J are intersubstituable)
    * - I < J if all x \in I, y \in J have x < y
    * - I > J if all x \in I, y \in J have x > y
    */
  class IntervalGeometricPartialOrder[A: Order] extends PartialOrder[Interval[A]] {
    override def eqv(x: Interval[A], y: Interval[A]): Boolean = (x == y)

    def partialCompare(i: Interval[A], j: Interval[A]): Double = {
      import Double.NaN
      if (eqv(i, j)) return 0.0
      if (i.isEmpty || j.isEmpty)
        return NaN

      // test if i < j
      (i.upperBound, j.lowerBound) match {
        case (Open(x), Open(y)) if x <= y => return -1
        case (Open(x), Closed(y)) if x <= y => return -1
        case (Closed(x), Open(y)) if x <= y => return -1
        case (Closed(x), Closed(y)) if x < y => return -1
        case _ =>
      }
      // test if i > j
        (i.lowerBound, j.upperBound) match {
        case (Open(x), Open(y)) if x >= y => return 1
        case (Open(x), Closed(y)) if x >= y => return 1
        case (Closed(x), Open(y)) if x >= y => return 1
        case (Closed(x), Closed(y)) if x > y => return 1
        case _ =>
      }
      return NaN
    }
  }

  implicit def intervalGeometricPartialOrder[A: Order]: PartialOrder[Interval[A]] = new IntervalGeometricPartialOrder[A]
}

object unicode {
  import spire.math._

  type ℍ = Quaternion[Real]
  type ℂ = Complex[Real]
  type ℝ = Real
  type ℚ = Rational
  type ℤ = SafeLong
  type ℕ = Natural

  val ⅇ = Real.e
  val π = Real.pi
  val φ = (Real(1) + Real(5).sqrt) / Real(2)
  val ⅈ = Complex.i[Real]
  val ⅉ = Quaternion.j[Real]

  def ⊤[A](implicit ev: BooleanAlgebra[A]): A = ev.one
  def ⊥[A](implicit ev: BooleanAlgebra[A]): A = ev.zero
  def ¬[A](a: A)(implicit ev: BooleanAlgebra[A]): A = ev.complement(a)
  def √[A](a: A)(implicit ev: NRoot[A]): A = ev.sqrt(a)
  def ∛[A](a: A)(implicit ev: NRoot[A]): A = ev.nroot(a, 3)
  def ∜[A](a: A)(implicit ev: NRoot[A]): A = ev.nroot(a, 4)

  def Σ[A](as: Iterable[A])(implicit ev: AdditiveMonoid[A]): A =
    as.foldLeft(ev.zero)(ev.plus)

  def Π[A](as: Iterable[A])(implicit ev: MultiplicativeMonoid[A]): A =
    as.foldLeft(ev.one)(ev.times)

  implicit class TimesOp[A](lhs: A)(implicit ev: MultiplicativeSemigroup[A]) {
    def ∙(rhs: A): A = ev.times(lhs, rhs)
  }

  implicit class EqOps[A](lhs: A)(implicit ev: Eq[A]) {
    def ≡(rhs: A): Boolean = ev.eqv(lhs, rhs)
    def ≠(rhs: A): Boolean = lhs != rhs
  }

  implicit class PartialOrderOps[A](lhs: A)(implicit ev: PartialOrder[A]) {
    def ≤(rhs: A): Boolean = ev.lteqv(lhs, rhs)
    def ≥(rhs: A): Boolean = ev.gteqv(lhs, rhs)
  }

  implicit class BooleanAlgebraOps[A](lhs: A)(implicit ev: BooleanAlgebra[A]) {
    def ∧(rhs: A): A = ev.and(lhs, rhs)
    def ∨(rhs: A): A = ev.or(lhs, rhs)

    def ⊻(rhs: A): A = ev.xor(lhs, rhs)
    def ⊼(rhs: A): A = ev.nand(lhs, rhs)
    def ⊽(rhs: A): A = ev.nor(lhs, rhs)

    def ⊃(rhs: A): A = ev.imp(lhs, rhs)
  }

  implicit def setOps[A](lhs: Interval[A]) = new Interval.SymbolicSetOps(lhs)
}

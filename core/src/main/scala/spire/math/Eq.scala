package spire.math

import scala.{specialized => spec}

import spire.algebra.Zero
import spire.macrosk.Ops
import scala.collection.SeqLike
import scala.reflect.ClassTag

trait Eq[@spec A] {
  def eqv(x:A, y:A): Boolean
  def neqv(x:A, y:A): Boolean = !eqv(x, y)
  def on[@spec B](f:B => A): Eq[B] = new MappedEq(this)(f)
}

class MappedEq[@spec A, @spec B](eq: Eq[B])(f: A => B) extends Eq[A] {
  def eqv(x: A, y: A): Boolean = eq.eqv(f(x), f(x))
}

final class EqOps[A](lhs:A)(implicit ev:Eq[A]) {
  def ===(rhs:A) = macro Ops.binop[A, Boolean]
  def =!=(rhs:A) = macro Ops.binop[A, Boolean]
}

object Eq extends Eq1 with EqProductImplicits {
  implicit object ByteEq extends ByteEq
  implicit object ShortEq extends ShortEq
  implicit object CharEq extends CharEq
  implicit object IntEq extends IntEq
  implicit object LongEq extends LongEq
  implicit object UByteEq extends UByteEq
  implicit object UShortEq extends UShortEq
  implicit object UIntEq extends UIntEq
  implicit object ULongEq extends ULongEq
  implicit object FloatEq extends FloatEq
  implicit object DoubleEq extends DoubleEq
  implicit object BigIntEq extends BigIntEq
  implicit object BigDecimalEq extends BigDecimalEq
  implicit object RationalEq extends RationalEq
  implicit def complexEq[A:Fractional] = new ComplexEq[A] {}
  implicit def gaussianEq[A: Integral] = new GaussianEq[A] {}
  implicit object RealEq extends RealEq
  implicit object SafeLongEq extends SafeLongEq
  implicit object NaturalEq extends NaturalEq
  implicit object NumberEq extends NumberEq
  implicit object StringEq extends StringEq

  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@spec A, @spec B](f:A => B)(implicit e:Eq[B]): Eq[A] = new MappedEq(e)(f)
}

trait Eq0 {
  implicit def seq[A, CC[A] <: SeqLike[A, CC[A]]](implicit A0: Eq[A]) = {
    new SeqEq[A, CC[A]] {
      val A = A0
    }
  }

  implicit def map[K, V](implicit V0: Eq[V]) = new MapEq[K, V] {
    val V = V0
  }

  implicit def option[A: Eq] = new OptionEq[A] {
    val A = Eq[A]
  }
}

trait Eq1 extends Eq0 {
  implicit def array[@spec(Int,Long,Float,Double) A](implicit
      A0: Eq[A], ct: ClassTag[A]) = new ArrayEq[A] {
    val A = A0
    val classTag = ct
  }
}

trait ByteEq extends Eq[Byte] {
  def eqv(x:Byte, y:Byte) = x == y
  override def neqv(x:Byte, y:Byte) = x != y
}
trait ShortEq extends Eq[Short] {
  def eqv(x:Short, y:Short) = x == y
  override def neqv(x:Short, y:Short) = x != y
}
trait CharEq extends Eq[Char] {
  def eqv(x:Char, y:Char) = x == y
  override def neqv(x:Char, y:Char) = x != y
}
trait IntEq extends Eq[Int] {
  def eqv(x:Int, y:Int) = x == y
  override def neqv(x:Int, y:Int) = x != y
}
trait LongEq extends Eq[Long] {
  def eqv(x:Long, y:Long) = x == y
  override def neqv(x:Long, y:Long) = x != y
}
trait UByteEq extends Eq[UByte] {
  def eqv(x:UByte, y:UByte) = x == y
  override def neqv(x:UByte, y:UByte) = x != y
}
trait UShortEq extends Eq[UShort] {
  def eqv(x:UShort, y:UShort) = x == y
  override def neqv(x:UShort, y:UShort) = x != y
}
trait UIntEq extends Eq[UInt] {
  def eqv(x:UInt, y:UInt) = x == y
  override def neqv(x:UInt, y:UInt) = x != y
}
trait ULongEq extends Eq[ULong] {
  def eqv(x:ULong, y:ULong) = x == y
  override def neqv(x:ULong, y:ULong) = x != y
}
trait FloatEq extends Eq[Float] {
  def eqv(x:Float, y:Float) = x == y
  override def neqv(x:Float, y:Float) = x != y
}
trait DoubleEq extends Eq[Double] {
  def eqv(x:Double, y:Double) = x == y
  override def neqv(x:Double, y:Double) = x != y
}
trait BigIntEq extends Eq[BigInt] {
  def eqv(x:BigInt, y:BigInt) = x == y
  override def neqv(x:BigInt, y:BigInt) = x != y
}
trait BigDecimalEq extends Eq[BigDecimal] {
  def eqv(x:BigDecimal, y:BigDecimal) = x == y
  override def neqv(x:BigDecimal, y:BigDecimal) = x != y
}
trait RationalEq extends Eq[Rational] {
  def eqv(x:Rational, y:Rational) = x == y
  override def neqv(x:Rational, y:Rational) = x != y
}
trait ComplexEq[A] extends Eq[Complex[A]] {
  def eqv(x:Complex[A], y:Complex[A]) = x eqv y
  override def neqv(x:Complex[A], y:Complex[A]) = x neqv y
}
trait GaussianEq[A] extends Eq[Gaussian[A]] {
  def eqv(x:Gaussian[A], y:Gaussian[A]) = x eqv y
  override def neqv(x:Gaussian[A], y:Gaussian[A]) = x neqv y
}
trait RealEq extends Eq[Real] {
  def eqv(x: Real, y: Real) = (x - y).sign == Zero
  override def neqv(x: Real, y: Real) = (x - y).sign != Zero
}
trait SafeLongEq extends Eq[SafeLong] {
  def eqv(x: SafeLong, y: SafeLong) = x == y
  override def neqv(x: SafeLong, y: SafeLong) = x != y
}
trait NumberEq extends Eq[Number] {
  def eqv(x: Number, y: Number) = x == y
  override def neqv(x: Number, y: Number) = x != y
}
trait NaturalEq extends Eq[Natural] {
  def eqv(x: Natural, y: Natural) = x == y
  override def neqv(x: Natural, y: Natural) = x != y
}
trait OptionEq[A] extends Eq[Option[A]] {
  def A: Eq[A]
  def eqv(x: Option[A], y: Option[A]) = (x, y) match {
    case (Some(x), Some(y)) => A.eqv(x, y)
    case (None, None) => true
    case _ => false
  }
}
trait StringEq extends Eq[String] {
  def eqv(x: String, y: String): Boolean = x == y
}

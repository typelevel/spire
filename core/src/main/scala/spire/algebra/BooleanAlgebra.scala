package spire.algebra

import scala.{specialized => spec}

import spire.math._
import spire.macrosk.Ops

/**
 * A boolean algebra is a structure that defines a few basic operations, namely
 * as conjunction (&), disjunction (|), and negation (~). Both conjunction and
 * disjunction must be associative, commutative and should distribute over each
 * other. Also, both have an identity. Also, they obey the absorption law, that
 * is `x & (y | x) == x` and `x | (x & y) == x`.
 */
trait BooleanAlgebra[@spec(Boolean, Byte, Short, Int, Long) A] { self =>
  def one: A
  def zero: A
  def complement(a: A): A
  def and(a: A, b: A): A
  def or(a: A, b: A): A
  def xor(a: A, b: A): A = or(and(a, complement(b)), and(complement(a), b))

  def imp(a: A, b: A): A = or(complement(a), b)
  def nand(a: A, b: A): A = complement(and(a, b))
  def nor(a: A, b: A): A = complement(or(a, b))
  def nxor(a: A, b: A): A = and(or(a, complement(b)), or(complement(a), b))

  def dual: BooleanAlgebra[A] = new BooleanAlgebra[A] {
    def one: A = self.zero
    def zero: A = self.one
    def and(a: A, b: A): A = self.or(a, b)
    def or(a: A, b: A): A = self.and(a, b)
    def complement(a: A): A = self.complement(a)
    override def xor(a: A, b: A): A = self.complement(self.xor(a, b))

    override def dual: BooleanAlgebra[A] = self
  }
}

final class BooleanAlgebraOps[A](lhs:A)(implicit ev:BooleanAlgebra[A]) {
  def unary_~() = macro Ops.unop[A]
  def &(rhs: A): A = macro Ops.binop[A, A]
  def |(rhs: A): A = macro Ops.binop[A, A]
  def ^(rhs: A): A = macro Ops.binop[A, A]

  def &(rhs: Int)(implicit c: ConvertableTo[A]): A = ev.and(lhs, c.fromInt(rhs))
  def |(rhs: Int)(implicit c: ConvertableTo[A]): A = ev.or(lhs, c.fromInt(rhs))
  def ^(rhs: Int)(implicit c: ConvertableTo[A]): A = ev.xor(lhs, c.fromInt(rhs))

  def &(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) & rhs
  def |(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) | rhs
  def ^(rhs: Number)(implicit c: ConvertableFrom[A]): Number = c.toNumber(lhs) ^ rhs
}

object BooleanAlgebra {
  @inline final def apply[@specialized(Boolean, Byte, Short, Int, Long) A](
    implicit ev: BooleanAlgebra[A]): BooleanAlgebra[A] = ev

  implicit object UByteIsBooleanAlgebra extends UByteIsBooleanAlgebra
  implicit object UShortIsBooleanAlgebra extends UShortIsBooleanAlgebra
  implicit object UIntIsBooleanAlgebra extends UIntIsBooleanAlgebra
  implicit object ULongIsBooleanAlgebra extends ULongIsBooleanAlgebra
}

trait UByteIsBooleanAlgebra extends BooleanAlgebra[UByte] {
  def one: UByte = UByte(-1: Byte)
  def zero: UByte = UByte(0: Byte)
  def and(a: UByte, b: UByte): UByte = a & b
  def or(a: UByte, b: UByte): UByte = a | b
  def complement(a: UByte): UByte = ~a
  override def xor(a: UByte, b: UByte): UByte = a ^ b
}

trait UShortIsBooleanAlgebra extends BooleanAlgebra[UShort] {
  def one: UShort = UShort(-1: Short)
  def zero: UShort = UShort(0: Short)
  def and(a: UShort, b: UShort): UShort = a & b
  def or(a: UShort, b: UShort): UShort = a | b
  def complement(a: UShort): UShort = ~a
  override def xor(a: UShort, b: UShort): UShort = a ^ b
}

trait UIntIsBooleanAlgebra extends BooleanAlgebra[UInt] {
  def one: UInt = UInt(-1)
  def zero: UInt = UInt(0)
  def and(a: UInt, b: UInt): UInt = a & b
  def or(a: UInt, b: UInt): UInt = a | b
  def complement(a: UInt): UInt = ~a
  override def xor(a: UInt, b: UInt): UInt = a ^ b
}

trait ULongIsBooleanAlgebra extends BooleanAlgebra[ULong] {
  def one: ULong = ULong(-1L)
  def zero: ULong = ULong(0L)
  def and(a: ULong, b: ULong): ULong = a & b
  def or(a: ULong, b: ULong): ULong = a | b
  def complement(a: ULong): ULong = ~a
  override def xor(a: ULong, b: ULong): ULong = a ^ b
}

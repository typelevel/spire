package spire.algebra

import scala.{specialized => spec}

import spire.math._
import spire.macrosk.Ops

/**
 * A boolean algebra is a structure that defines a few basic operations, namely
 * as conjunction (&), disjunction (|), and negation (~). Both conjunction and
 * disjunction must be associative, commutative and should distrubte over each
 * other. Also, both have an identity. Also, they obey the absortion laws, that
 * is `x & (y | x) == x` and `x | (x & y) == x`.
 */
trait BooleanAlgebra[@spec(Boolean, Byte, Short, Int, Long) A] {
  def one: A
  def zero: A
  def and(a: A, b: A): A
  def or(a: A, b: A): A
  def complement(a: A): A
  def xor(a: A, b: A): A = or(and(a, complement(b)), and(complement(a), b))
}

final class BooleanAlgebraOps[A](lhs:A)(implicit ev:BooleanAlgebra[A]) {
  def unary_~() = macro Ops.unop[A]
  def &(rhs: A): A = macro Ops.binop[A, A]
  def |(rhs: A): A = macro Ops.binop[A, A]
  def ^(rhs: A): A = macro Ops.binop[A, A]
}

object BooleanAlgebra {
  implicit object BooleanIsBooleanAlgebra extends BooleanIsBooleanAlgebra
  implicit object ByteIsBooleanAlgebra extends ByteIsBooleanAlgebra
  implicit object ShortIsBooleanAlgebra extends ShortIsBooleanAlgebra
  implicit object IntIsBooleanAlgebra extends IntIsBooleanAlgebra
  implicit object LongIsBooleanAlgebra extends LongIsBooleanAlgebra
  implicit object UByteIsBooleanAlgebra extends UByteIsBooleanAlgebra
  implicit object UShortIsBooleanAlgebra extends UShortIsBooleanAlgebra
  implicit object UIntIsBooleanAlgebra extends UIntIsBooleanAlgebra
  implicit object ULongIsBooleanAlgebra extends ULongIsBooleanAlgebra
}

trait BooleanIsBooleanAlgebra extends BooleanAlgebra[Boolean] {
  def one: Boolean = true
  def zero: Boolean = false
  def and(a: Boolean, b: Boolean): Boolean = a & b
  def or(a: Boolean, b: Boolean): Boolean = a | b
  def complement(a: Boolean): Boolean = !a
  override def xor(a: Boolean, b: Boolean): Boolean = a ^ b
}

trait ByteIsBooleanAlgebra extends BooleanAlgebra[Byte] {
  def one: Byte = (-1: Byte)
  def zero: Byte = (0: Byte)
  def and(a: Byte, b: Byte): Byte = (a & b).toByte
  def or(a: Byte, b: Byte): Byte = (a | b).toByte
  def complement(a: Byte): Byte = (~a).toByte
  override def xor(a: Byte, b: Byte): Byte = (a ^ b).toByte
}

trait ShortIsBooleanAlgebra extends BooleanAlgebra[Short] {
  def one: Short = (-1: Short)
  def zero: Short = (0: Short)
  def and(a: Short, b: Short): Short = (a & b).toShort
  def or(a: Short, b: Short): Short = (a | b).toShort
  def complement(a: Short): Short = (~a).toShort
  override def xor(a: Short, b: Short): Short = (a ^ b).toShort
}

trait IntIsBooleanAlgebra extends BooleanAlgebra[Int] {
  def one: Int = -1
  def zero: Int = 0
  def and(a: Int, b: Int): Int = a & b
  def or(a: Int, b: Int): Int = a | b
  def complement(a: Int): Int = ~a
  override def xor(a: Int, b: Int): Int = a ^ b
}

trait LongIsBooleanAlgebra extends BooleanAlgebra[Long] {
  def one: Long = -1L
  def zero: Long = 0L
  def and(a: Long, b: Long): Long = a & b
  def or(a: Long, b: Long): Long = a | b
  def complement(a: Long): Long = ~a
  override def xor(a: Long, b: Long): Long = a ^ b
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
  def one: UInt = -1
  def zero: UInt = 0
  def and(a: UInt, b: UInt): UInt = a & b
  def or(a: UInt, b: UInt): UInt = a | b
  def complement(a: UInt): UInt = ~a
  override def xor(a: UInt, b: UInt): UInt = a ^ b
}

trait ULongIsBooleanAlgebra extends BooleanAlgebra[ULong] {
  def one: ULong = -1L
  def zero: ULong = 0L
  def and(a: ULong, b: ULong): ULong = a & b
  def or(a: ULong, b: ULong): ULong = a | b
  def complement(a: ULong): ULong = ~a
  override def xor(a: ULong, b: ULong): ULong = a ^ b
}



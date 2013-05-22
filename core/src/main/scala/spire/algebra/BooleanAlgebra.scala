package spire.algebra

import scala.{specialized => spec}

import spire.math.{ ConvertableFrom, ConvertableTo, Number }

/**
 * A boolean algebra is a structure that defines a few basic operations, namely
 * as conjunction (&), disjunction (|), and negation (~). Both conjunction and
 * disjunction must be associative, commutative and should distribute over each
 * other. Also, both have an identity and they obey the absorption law; that
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

object BooleanAlgebra {
  @inline final def apply[@specialized(Boolean, Byte, Short, Int, Long) A](
    implicit ev: BooleanAlgebra[A]): BooleanAlgebra[A] = ev
}

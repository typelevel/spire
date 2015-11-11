package spire
package algebra

import spire.algebra.lattice.Heyting

/**
 * A boolean algebra is a structure that defines a few basic operations, namely
 * as conjunction (&), disjunction (|), and negation (~). Both conjunction and
 * disjunction must be associative, commutative and should distribute over each
 * other. Also, both have an identity and they obey the absorption law; that
 * is `x & (y | x) == x` and `x | (x & y) == x`.
 */
trait Bool[@sp(Boolean, Byte, Short, Int, Long) A] extends Any with Heyting[A] {
  def xor(a: A, b: A): A = or(and(a, complement(b)), and(complement(a), b))
  def imp(a: A, b: A): A = or(complement(a), b)
  def nand(a: A, b: A): A = complement(and(a, b))
  def nor(a: A, b: A): A = complement(or(a, b))
  def nxor(a: A, b: A): A = and(or(a, complement(b)), or(complement(a), b))

  def dual: Bool[A] = new DualBool(this)
}

class DualBool[@sp(Boolean, Byte, Short, Int, Long) A](orig: Bool[A]) extends Bool[A] {
  def one: A = orig.zero
  def zero: A = orig.one
  def and(a: A, b: A): A = orig.or(a, b)
  def or(a: A, b: A): A = orig.and(a, b)
  def complement(a: A): A = orig.complement(a)
  override def xor(a: A, b: A): A = orig.complement(orig.xor(a, b))

  override def imp(a: A, b: A): A = orig.and(orig.complement(a), b)
  override def nand(a: A, b: A): A = orig.nor(a, b)
  override def nor(a: A, b: A): A = orig.nand(a, b)
  override def nxor(a: A, b: A): A = orig.xor(a, b)

  override def dual: Bool[A] = orig
}

object Bool {
  @inline final def apply[@sp(Boolean, Byte, Short, Int, Long) A](implicit ev: Bool[A]): Bool[A] = ev
}

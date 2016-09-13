package spire
package math

import spire.algebra.Bool

trait BitString[@sp(Byte, Short, Int, Long) A] extends Any with Bool[A] {
  def signed: Boolean
  def width: Int
  def toHexString(n: A): String

  def bitCount(n: A): Int
  def highestOneBit(n: A): A
  def lowestOneBit(n: A): A
  def numberOfLeadingZeros(n: A): Int
  def numberOfTrailingZeros(n: A): Int

  def leftShift(n: A, i: Int): A
  def rightShift(n: A, i: Int): A
  def signedRightShift(n: A, i: Int): A
  def rotateLeft(n: A, i: Int): A
  def rotateRight(n: A, i: Int): A
}

object BitString {
  def apply[A](implicit ev: BitString[A]): BitString[A] = ev
}

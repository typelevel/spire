package spire.math

import spire.macrosk.Ops
import spire.algebra.Bool

import scala.{specialized => spec}
import java.lang.Math

trait BitString[@spec(Byte, Short, Int, Long) A] extends Bool[A] {
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

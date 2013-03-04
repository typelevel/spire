package spire.std

import spire.algebra._

trait IntIsRing extends Ring[Int] {
  override def minus(a:Int, b:Int): Int = a - b
  def negate(a:Int): Int = -a
  def one: Int = 1
  def plus(a:Int, b:Int): Int = a + b
  override def pow(a:Int, b:Int): Int = Math.pow(a, b).toInt
  override def times(a:Int, b:Int): Int = a * b
  def zero: Int = 0

  override def fromInt(n: Int): Int = n
}

trait IntIsEuclideanRing extends EuclideanRing[Int] with IntIsRing {
  def quot(a:Int, b:Int) = a / b
  def mod(a:Int, b:Int) = a % b
  def gcd(a:Int, b:Int): Int = spire.math.gcd(a, b).toInt
}

trait IntEq extends Eq[Int] {
  def eqv(x:Int, y:Int) = x == y
  override def neqv(x:Int, y:Int) = x != y
}

// Not included in Instances trait.
trait IntIsNRoot extends NRoot[Int] {
  def nroot(x: Int, n: Int): Int = {
    def findnroot(prev: Int, add: Int): Int = {
      val next = prev | add
      val e = Math.pow(next, n)

      if (e == x || add == 0) {
        next
      } else if (e <= 0 || e > x) {
        findnroot(prev, add >> 1)
      } else {
        findnroot(next, add >> 1)
      }
    }

    findnroot(0, 1 << ((33 - n) / n))
  }

  def log(a:Int) = Math.log(a.toDouble).toInt
  def fpow(a:Int, b:Int) = Math.pow(a, b).toInt
}

trait IntOrder extends Order[Int] with IntEq {
  override def gt(x: Int, y: Int) = x > y
  override def gteqv(x: Int, y: Int) = x >= y
  override def lt(x: Int, y: Int) = x < y
  override def lteqv(x: Int, y: Int) = x <= y
  def compare(x: Int, y: Int) = if (x < y) -1 else if (x > y) 1 else 0
}

trait IntIsSigned extends Signed[Int] {
  def signum(a: Int): Int = a
  def abs(a: Int): Int = if (a < 0) -a else a
}

trait IntIsReal extends IsReal[Int] with IntOrder with IntIsSigned {
  def toDouble(n: Int): Double = n.toDouble
}

trait IntIsBooleanAlgebra extends BooleanAlgebra[Int] {
  def one: Int = -1
  def zero: Int = 0
  def and(a: Int, b: Int): Int = a & b
  def or(a: Int, b: Int): Int = a | b
  def complement(a: Int): Int = ~a
  override def xor(a: Int, b: Int): Int = a ^ b
}

trait IntInstances {
  implicit object IntBooleanAlgebra extends IntIsBooleanAlgebra
  implicit object IntAlgebra extends IntIsEuclideanRing with IntIsNRoot
  implicit object IntIsReal extends IntIsReal
}

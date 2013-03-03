package spire.algebra
package std

trait LongIsRing extends Ring[Long] {
  override def minus(a:Long, b:Long): Long = a - b
  def negate(a:Long): Long = -a
  def one: Long = 1L
  def plus(a:Long, b:Long): Long = a + b
  override def pow(a: Long, b:Int): Long = b match {
    case 0 => 1
    case 1 => a
    case 2 => a * a
    case 3 => a * a * a
    case _ =>
      if (b > 0) {
        val e = b >> 1
        val c = if ((b & 1) == 1) a else 1
        c * pow(a, e) * pow(a, e)
      } else {
        0
      }
  }
  override def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L
  
  override def fromInt(n: Int): Long = n
}

trait LongIsEuclideanRing extends EuclideanRing[Long] with LongIsRing {
  def quot(a:Long, b:Long) = a / b
  def mod(a:Long, b:Long) = a % b
  def gcd(a:Long, b:Long) = spire.math.gcd(a, b)
}

trait LongEq extends Eq[Long] {
  def eqv(x:Long, y:Long) = x == y
  override def neqv(x:Long, y:Long) = x != y
}

// Not included in Instances trait!
trait LongIsNRoot extends NRoot[Long] {
  def nroot(x: Long, n: Int): Long = {
    def findnroot(prev: Long, add: Long): Long = {
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

    findnroot(0, 1L << ((65 - n) / n))
  }
  def log(a:Long) = Math.log(a.toDouble).toLong
  def fpow(a:Long, b:Long) = spire.math.pow(a, b) // xyz
}

trait LongOrder extends Order[Long] with LongEq {
  override def gt(x: Long, y: Long) = x > y
  override def gteqv(x: Long, y: Long) = x >= y
  override def lt(x: Long, y: Long) = x < y
  override def lteqv(x: Long, y: Long) = x <= y
  def compare(x: Long, y: Long) = if (x < y) -1 else if (x > y) 1 else 0
}

trait LongIsSigned extends Signed[Long] {
  def signum(a: Long): Int = a.toInt
  def abs(a: Long): Long = if (a < 0L) -a else a
}

trait LongIsReal extends LongOrder with LongIsSigned {
  def toDouble(n: Long): Double = n.toDouble
}

trait LongIsBooleanAlgebra extends BooleanAlgebra[Long] {
  def one: Long = -1L
  def zero: Long = 0L
  def and(a: Long, b: Long): Long = a & b
  def or(a: Long, b: Long): Long = a | b
  def complement(a: Long): Long = ~a
  override def xor(a: Long, b: Long): Long = a ^ b
}

trait LongInstances {
  object LongIsNRoot extends LongIsNRoot
  implicit object LongBooleanAlgebra extends LongIsBooleanAlgebra
  implicit object LongAlgebra extends LongIsEuclideanRing
  implicit object LongIsReal extends LongIsReal
}

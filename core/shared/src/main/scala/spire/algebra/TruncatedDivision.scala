package spire
package algebra

/**
 * Division and modulus for computer scientists
 * taken from https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
 * 
 * For two numbers x (dividend) and y (divisor) on an ordered ring with y != 0,
 * there exists a pair of numbers q (quotient) and r (remainder)
 * such that these laws are satisfied:
 * 
 * (1) q is an integer
 * (2) x = y * q + r (division rule)
 * (3) |r| < |y|,
 * (4t) r = 0 or sign(r) = sign(x),
 * (4f) r = 0 or sign(r) = sign(y).
 * 
 * where sign is the sign function, and the absolute value 
 * function |x| is defined as |x| = x if x >=0, and |x| = -x otherwise.
 * 
 * We define functions tmod and tdiv such that:
 * q = tdiv(x, y) and r = tmod(x, y) obey rule (4t)
 * and functions fmod and fdiv such that:
 * q = fdiv(x, y) and r = fmod(x, y) obey rule (4f)
 * 
 * Law (4t) corresponds to ISO C99 and Haskell's quot/rem.
 * Law (4f) is described by Knuth and used by Haskell,
 * and fmod corresponds to the REM function of the IEEE floating-point standard.
 */
trait TruncatedDivision[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Signed[A] {

//  def isWhole(x: A): Boolean
  def tdiv(x: A, y: A): BigInt
  def tmod(x: A, y: A): A
  def tdivmod(x: A, y: A): (BigInt, A) = (tdiv(x, y), tmod(x, y))

  def fmod(x: A, y: A): A
  def fdiv(x: A, y: A): BigInt
  def fdivmod(x: A, y: A): (BigInt, A)

}

trait TruncatedDivisionCRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with TruncatedDivision[A] with CRing[A] {

  def fmod(x: A, y: A): A = {
    val (tq, tm) = tdivmod(x, y)
    if (signum(tm) == -signum(y)) plus(tm, y) else tm
  }

  def fdiv(x: A, y: A): BigInt = {
    val (tq, tm) = tdivmod(x, y)
    if (signum(tm) == -signum(y)) tq - 1 else tq
  }

  def fdivmod(x: A, y: A): (BigInt, A) = {
    val (tq, tm) = tdivmod(x, y)
    val i = if (signum(tm) == -signum(y)) 1 else 0
    val fq = tq - i
    val fm = if (i == 1) plus(tm, y) else tm
    (fq, fm)
  }

}

package spire.math.fpf

import spire.math._

import java.lang.Float.floatToIntBits


/**
 * A `Long` encoded version of `MaybeDouble`, geared for unboxed speed. This
 * is significantly less accurate, but may provide good speed gains in common
 * cases.
 *
 * The values `approx` and `measure` are both truncated floats with the full
 * 8 bit exponent, but only 16 bit mantissa. Since the `measure` is always
 * positive, we only need `2 * (16 + 8) + 1 = 49` bits for these 2 numbers,
 * which leaves 15 bits for the `index`. These sizes were chosen so that
 * worst case relative error, `ind * eps`, has a value less than 1. In our
 * case, with worst case `ind = -1 >>> 17`, we have `ind * eps =~ 0.5`.
 *
 * This was inspired by Erik Osheim's awesome FastComplex, which encodes the
 * imaginary and real parts as 2 `Float`s in the a single `Long`.
 *
 * TODO: Perhaps, if we've only used + or *, we could store a higher precision
 *       float instead, since we don't need measure. Use 1 bit to handle the
 *       switching, taken from the index.
 */
object FastMaybeFloat {
  final val Invalid = -1L
 
  // esp = ~0.000015
  final val eps = java.lang.Float.intBitsToFloat((127 - 16) << 23)

  private final val indexMask   = 0x0000000000007FFFL
  private final val measureMask = 0x0000007FFFFF8000L
  private final val approxMask  = 0xFFFFFF8000000000L
  private final val invalidMask = 0x7F80000000000000L

  @inline def asFloat(x: Long): Float = java.lang.Float.intBitsToFloat(x.toInt)
  @inline def max(a: Int, b: Int): Int = if (a > b) a else b
  @inline def min(a: Int, b: Int): Int = if (a < b) a else b
  @inline def invalid(a: Long): Boolean = (a & invalidMask) == invalidMask

  final def approx(mf: Long): Float = asFloat((mf & approxMask) >>> 32)
  @inline final def measure(mf: Long): Float = asFloat((mf & measureMask) >>> 8)
  @inline final def index(mf: Long): Int = (mf & indexMask).toInt
  final def error(mf: Long): Float = measure(mf) * index(mf) * eps

  @inline private final def applySafe(a: Float, m: Float, i: Int): Long = 
    apply(a, m, min(i, 1 << 16))
  
  /**
   * This should be used whenever it can be guaranteed that `i < 2^17`. Notably,
   * this can be used when adding at most 1 to the sum of 2 different indexes.
   */
  private final def apply(a: Float, m: Float, i: Int): Long = {
    val x = ((floatToIntBits(a).toLong << 17) & 0xFFFFFFFFFF000000L) |
      (floatToIntBits(m) >>> 7).toLong
    
    // Sign extends it if bit 16 is set, thus making it invalid.
    
    (x << 15) | (i.toLong << 48 >> 48)
  }


  final def plus(a: Long, b: Long): Long =
    apply(approx(a) + approx(b), measure(a) + measure(b), max(index(a), index(b)) + 1)

  final def minus(a: Long, b: Long): Long =
    apply(approx(a) - approx(b), measure(a) + measure(b), max(index(a), index(b)) + 1)

  final def times(a: Long, b: Long): Long =
    apply(approx(a) * approx(b), measure(a) * measure(b), index(a) + index(b) + 1)

  final def div(a: Long, b: Long): Long = {
    val fa = approx(a)
    val fb = approx(b)
    val ma = measure(a)
    val mb = measure(b)
    val ib = index(b)

    apply(
      fa / fb,
      (abs(fa) / abs(fb) + ma / mb) / (abs(fb) / mb - (ib + 1) * eps),
      1 + max(index(a), ib + 1)
    )
  }

  def pow(a: Long, k: Int): Long = if (k > 0) {
    apply(math.pow(approx(a), k).toFloat, math.pow(measure(a), k).toFloat, index(a) + k)
  } else if (k < 0) {
    val fb = math.pow(approx(a), -k).toFloat
    val mb = math.pow(measure(a), -k).toFloat
    val ib = index(a) + k

    val mes = (1 / abs(fb) + 1 / mb) / (abs(fb) / mb - (mb + 1) * eps)

    apply(1 / fb, mes, ib + 2)
  } else {
    apply(1, 1, 0)
  }
 
  def nroot(a: Long, n: Int): Long = if (n == 2) sqrt(a) else Invalid
   
  def sqrt(a: Long): Long = {
    val fa = approx(a)

    // If fa < 0, then sqrt(fa) == NaN, which is OK for now.

    if (invalid(a) || fa <= 0.0f) {
      apply(math.sqrt(fa).toFloat, math.sqrt(measure(a)).toFloat * (1 << 4), index(a) + 1)
    } else {
      val fb = math.sqrt(fa).toFloat
      apply(fb, (measure(a) / fa) * fb, index(a) + 1)
    }
  }


  def apply(x: Float): Long = apply(x, x, 1)
  def apply(x: Double): Long = apply(x.toFloat, x.toFloat, 1)
  def apply(x: Int): Long = apply(x, x, 1)
  def apply(x: Long): Long = apply(x, x, 1)
  def apply(x: BigInt): Long = {
    val f = x.toFloat
    apply(f, f, 1)
  }
  def apply(x: BigDecimal): Long = {
    val f = x.toFloat
    apply(f, f, 1)
  }
  def apply(x: Rational): Long = {
    val f = x.toFloat
    apply(f, f, 1)
  }
}


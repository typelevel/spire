/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

object FloatComplex {
  import FastComplex.encode

  final def apply(real: Float, imag: Float): FloatComplex =
    new FloatComplex(encode(real, imag))

  final def apply(real: Double, imag: Double): FloatComplex =
    new FloatComplex(encode(real.toFloat, imag.toFloat))

  def polar(magnitude: Float, angle: Float): FloatComplex =
    new FloatComplex(FastComplex.polar(magnitude, angle))

  final val i: FloatComplex = new FloatComplex(4575657221408423936L)
  final val one: FloatComplex = new FloatComplex(1065353216L)
  final val zero: FloatComplex = new FloatComplex(0L)
}

/**
 * Value class which encodes two floating point values in a Long.
 *
 * We get (basically) unboxed complex numbers using this hack. The underlying implementation lives in the FastComplex
 * object.
 */
class FloatComplex(val u: Long) extends AnyVal {
  final override def toString: String = "(%s+%si)".format(real, imag)

  final def real: Float = FastComplex.real(u)
  final def imag: Float = FastComplex.imag(u)
  final def repr: String = "FloatComplex(%s, %s)".format(real, imag)
  final def abs: Float = FastComplex.abs(u)
  final def angle: Float = FastComplex.angle(u)
  final def conjugate: FloatComplex = new FloatComplex(FastComplex.conjugate(u))
  final def isWhole: Boolean = FastComplex.isWhole(u)
  final def signum: Int = FastComplex.signum(u)
  final def complexSignum: FloatComplex = new FloatComplex(FastComplex.complexSignum(u))
  final def negate: FloatComplex = new FloatComplex(FastComplex.negate(u))

  final def +(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.add(u, b.u))
  final def -(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.subtract(u, b.u))
  final def *(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.multiply(u, b.u))
  final def /(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.divide(u, b.u))

  /* TODO: does it make sense? Should match the behavior on Gaussian integers.
final def /~(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.quot(u, b.u))
final def %(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.mod(u, b.u))

final def /%(b: FloatComplex): (FloatComplex, FloatComplex) = FastComplex.quotmod(u, b.u) match {
  case (q, m) => (new FloatComplex(q), new FloatComplex(m))
}
   */

  final def pow(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.pow(u, b.u))
  final def **(b: FloatComplex): FloatComplex = pow(b)

  final def pow(b: Int): FloatComplex = new FloatComplex(FastComplex.pow(u, FastComplex(b.toFloat, 0.0f)))
  final def **(b: Int): FloatComplex = pow(b)
}

/**
 * FastComplex is an ugly, beautiful hack.
 *
 * The basic idea is to encode two 32-bit Floats into a single 64-bit Long. The lower-32 bits are the "real" Float and
 * the upper-32 are the "imaginary" Float.
 *
 * Since we're overloading the meaning of Long, all the operations have to be defined on the FastComplex object, meaning
 * the syntax for using this is a bit ugly. To add to the ugly beauty of the whole thing I could imagine defining
 * implicit operators on Long like +@, -@, *@, /@, etc.
 *
 * You might wonder why it's even worth doing this. The answer is that when you need to allocate an array of e.g. 10-20
 * million complex numbers, the GC overhead of using *any* object is HUGE. Since we can't build our own "pass-by-value"
 * types on the JVM we are stuck doing an encoding like this.
 *
 * Here are some profiling numbers for summing an array of complex numbers, timed against a concrete case class
 * implementation using Float (in ms):
 *
 * {{{
 * size | encoded |  class
 *   1M |     5.1 |    5.8
 *   5M |    28.5 |   91.7
 *   10M |    67.7 |  828.1
 *   20M |   228.0 | 2687.0
 * }}}
 *
 * Not bad, eh?
 */
object FastComplex {
  import java.lang.Math.{atan2, cos, sin}

  // note the superstitious use of @inline and final everywhere

  final def apply(real: Float, imag: Float): Long = encode(real, imag)
  final def apply(real: Double, imag: Double): Long = encode(real.toFloat, imag.toFloat)

  // encode a float as some bits
  @inline final def bits(n: Float): Int = java.lang.Float.floatToIntBits(n)

  // decode some bits into a float
  @inline final def bits(n: Int): Float = java.lang.Float.intBitsToFloat(n)

  // get the real part of the complex number
  @inline final def real(d: Long): Float = bits((d & 0xffffffff).toInt)

  // get the imaginary part of the complex number
  @inline final def imag(d: Long): Float = bits((d >>> 32).toInt)

  // define some handy constants
  final val i: Long = encode(0.0f, 1.0f)
  final val one: Long = encode(1.0f, 0.0f)
  final val zero: Long = encode(0.0f, 0.0f)

  // encode two floats representing a complex number
  @inline final def encode(real: Float, imag: Float): Long =
    bits(real) & 0xffffffffL | (bits(imag) & 0xffffffffL) << 32

  // encode two floats representing a complex number in polar form
  @inline final def polar(magnitude: Float, angle: Float): Long =
    encode(magnitude * cos(angle).toFloat, magnitude * sin(angle).toFloat)

  // decode should be avoided in fast code because it allocates a Tuple2.
  final def decode(d: Long): (Float, Float) = (real(d), imag(d))

  // produces a string representation of the Long/(Float,Float)
  final def toRepr(d: Long): String = "FastComplex(%s -> %s)".format(d, decode(d))

  // get the magnitude/absolute value
  final def abs(d: Long): Float = {
    val re = real(d)
    val im = imag(d)
    java.lang.Math.sqrt(re * re + im * im).toFloat
  }

  // get the angle/argument
  final def angle(d: Long): Float = atan2(imag(d), real(d)).toFloat

  // get the complex conjugate
  final def conjugate(d: Long): Long = encode(real(d), -imag(d))

  // see if the complex number is a whole value
  final def isWhole(d: Long): Boolean = real(d) % 1.0f == 0.0f && imag(d) % 1.0f == 0.0f

  // get the sign of the complex number
  final def signum(d: Long): Int = real(d).compare(0.0f)

  // get the complex sign of the complex number
  final def complexSignum(d: Long): Long = {
    val m = abs(d)
    if (m == 0.0f) zero else divide(d, encode(m, 0.0f))
  }

  // negation
  final def negate(a: Long): Long = encode(-real(a), -imag(a))

  // addition
  final def add(a: Long, b: Long): Long = encode(real(a) + real(b), imag(a) + imag(b))

  // subtraction
  final def subtract(a: Long, b: Long): Long = encode(real(a) - real(b), imag(a) - imag(b))

  // multiplication
  final def multiply(a: Long, b: Long): Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)
    encode(re_a * re_b - im_a * im_b, im_a * re_b + re_a * im_b)
  }

  // division
  final def divide(a: Long, b: Long): Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)

    val abs_re_b = Math.abs(re_b)
    val abs_im_b = Math.abs(im_b)

    if (abs_re_b >= abs_im_b) {
      if (abs_re_b == 0.0f) throw new ArithmeticException("/0")
      val ratio = im_b / re_b
      val denom = re_b + im_b * ratio
      encode((re_a + im_a * ratio) / denom, (im_a - re_a * ratio) / denom)

    } else {
      if (abs_im_b == 0.0f) throw new ArithmeticException("/0")
      val ratio = re_b / im_b
      val denom = re_b * ratio + im_b
      encode((re_a * ratio + im_a) / denom, (im_a * ratio - re_a) / denom)
    }
  }

  /* TODO: does it make sense? Should match the behvaior on Gaussian integers
   final def quot(a: Long, b: Long): Long =
    encode(Math.floor(real(divide(a, b))).toFloat, 0.0F)

  final def mod(a: Long, b: Long): Long = subtract(a, multiply(b, quot(a, b)))

  final def quotmod(a: Long, b: Long): (Long, Long) = {
    val q = quot(a, b)
    (q, subtract(a, multiply(b, quot(a, b))))
  }
   */

  // exponentiation
  final def pow(a: Long, b: Long): Long = if (b == zero) {
    encode(1.0f, 0.0f)

  } else if (a == zero) {
    if (imag(b) != 0.0f || real(b) < 0.0f)
      throw new Exception("raising 0 to negative/complex power")
    zero

  } else if (imag(b) != 0.0f) {
    val im_b = imag(b)
    val re_b = real(b)
    val len = (Math.pow(abs(a), re_b) / exp(angle(a) * im_b)).toFloat
    val phase = (angle(a) * re_b + log(abs(a)) * im_b).toFloat
    polar(len, phase)

  } else {
    val len = Math.pow(abs(a), real(b)).toFloat
    val phase = (angle(a) * real(b)).toFloat
    polar(len, phase)
  }
}

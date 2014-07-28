package spire.math

import spire.algebra.Heyting

/**
 * Implementation of three-valued logic.
 * 
 * This type resembles Boolean, but has three values instead of two:
 * 
 *  - Trilean.True (equivalent to true)
 *  - Trilean.False (equivalent to false)
 *  - Trilean.Unknown
 * 
 * Trilean supports the same operations that Boolean does, and as long
 * as all values are True or False, the results will be the
 * same. However, the truth tables have to be extended to work with
 * unknown:
 * 
 * not:
 * -+-
 * T|F
 * U|U
 * F|T
 * 
 * and:
 *  |T U F
 * -+-----
 * T|T U F
 * U|U U F
 * F|F F F
 * 
 * or:
 *  |T U F
 * -+-----
 * T|T T T
 * U|T U U
 * F|T U F
 * 
 * Trilean is implemented as a value type, so in most cases it will
 * only have the overhead of a single Int. However, in some situations
 * it will be boxed.
 */
class Trilean (val value: Int) extends AnyVal { lhs =>
  def isTrue: Boolean = value == -1
  def isFalse: Boolean = value == 0
  def isUnknown: Boolean = value == 1

  def isKnown: Boolean = value != 1
  def isNotTrue: Boolean = value != -1
  def isNotFalse: Boolean = value != 0

  def fold[A](f: Boolean => A)(unknown: => A): A =
    if (value == 1) unknown else f(value == -1)

  def assumeTrue: Boolean =
    value != 0

  def assumeFalse: Boolean =
    value == -1

  def assume(b: Boolean): Boolean =
    if (value == 1) b else value == -1

  def toBoolean(b: => Boolean): Boolean =
    if (value == 1) b else value == -1

  def toOption: Option[Boolean] =
    if (value == 1) None else Some(value == -1)

  override def toString: String =
    if (value == -1) "true" else if (value == 0) "false" else "unknown"

  def &&(rhs: => Trilean): Trilean =
    if (lhs.value == 0) lhs else lhs & rhs

  def ||(rhs: => Trilean): Trilean =
    if (lhs.value == -1) lhs else lhs | rhs

  def unary_! : Trilean =
    if (value == 1) lhs else new Trilean(~lhs.value)

  //   T U F
  // T T U F
  // U U U F
  // F F F F
  def &(rhs: Trilean): Trilean =
    new Trilean(lhs.value & rhs.value)

  //   T U F
  // T T T T
  // U T U U
  // F T U F
  def |(rhs: Trilean): Trilean =
    new Trilean(lhs.value | rhs.value)

  //   T U F
  // T F U T
  // U U U U
  // F T U F
  def ^(rhs: Trilean): Trilean =
    if (lhs.value == 1) lhs
    else if (rhs.value == 1) rhs
    else new Trilean(lhs.value ^ rhs.value)

  //   T U F
  // T T U F
  // U T U U
  // F T T T
  def imp(rhs: Trilean): Trilean =
    (!lhs) | rhs

  def nand(rhs: Trilean): Trilean =
    !(lhs & rhs)

  def nor(rhs: Trilean): Trilean =
    !(lhs | rhs)

  //   T U F
  // T T U F
  // U U U U
  // F F U T
  def nxor(rhs: Trilean): Trilean =
    if (lhs.value == 1) lhs
    else if (rhs.value == 1) rhs
    else new Trilean(~(lhs.value ^ rhs.value))
}

object Trilean {
  final val True: Trilean = new Trilean(-1)
  final val False: Trilean = new Trilean(0)
  final val Unknown: Trilean = new Trilean(1)

  final def apply(b: Boolean): Trilean =
    if (b) True else False

  final def apply(o: Option[Boolean]): Trilean =
    o.map(Trilean(_)).getOrElse(Unknown)

  final def apply(t: scala.util.Try[Boolean]): Trilean =
    t.map(Trilean(_)).getOrElse(Unknown)

  final def liftPf[A](p0: PartialFunction[A, Boolean]): A => Trilean = {
    val p = p0.andThen(Trilean(_))
    (a: A) => p.applyOrElse(a, (_: A) => Unknown)
  }

  final def testRef[A <: AnyRef](a: A)(f: A => Boolean): Trilean =
    if (a == null) Unknown else Trilean(f(a))

  final def testFloat(n: Float)(f: Float => Boolean): Trilean =
    if (java.lang.Float.isNaN(n)) Unknown else Trilean(f(n))

  final def testDouble(n: Double)(f: Double => Boolean): Trilean =
    if (java.lang.Double.isNaN(n)) Unknown else Trilean(f(n))

  final def run(body: => Boolean): Trilean =
    try {
      apply(body)
    } catch {
      case _: Exception => Unknown
    }

  implicit val algebra = new TrileanAlgebra
}

class TrileanAlgebra extends Heyting[Trilean] {
  def one: Trilean = Trilean.True
  def zero: Trilean = Trilean.True
  def complement(a: Trilean): Trilean = !a
  def and(a: Trilean, b: Trilean): Trilean = a & b
  def or(a: Trilean, b: Trilean): Trilean = a | b
  def imp(a: Trilean, b: Trilean): Trilean = a imp b
}

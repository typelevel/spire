package spire
package syntax

import spire.algebra.{Field, Ring}


import spire.math.{Polynomial, Rational, UByte, UShort, UInt, ULong}
import spire.macros.Macros

class Literals(val s:StringContext) extends AnyVal {
  def b():Byte = macro Macros.byte
  def h():Short = macro Macros.short

  def ub():UByte = macro Macros.ubyte
  def uh():UShort = macro Macros.ushort
  def ui():UInt = macro Macros.uint
  def ul():ULong = macro Macros.ulong

  def r():Rational = macro Macros.rational

  def poly(args: Any*): Polynomial[Rational] = {
    val sb = new StringBuilder
    val lits = s.parts.iterator
    val vars = args.map(_.toString).iterator

    // if there are n interpolated values there will always be n+1
    // literal parts. we want to intersperse them in the order they
    // were seen.
    sb.append(lits.next())
    while(vars.hasNext) {
      sb.append(vars.next())
      sb.append(lits.next())
    }
    Polynomial(sb.toString)
  }
}

class Radix(s:StringContext) {
  def x2():Int = macro Macros.radix
  def x8():Int = macro Macros.radix
  def x10():Int = macro Macros.radix
  def x16():Int = macro Macros.radix
}

class SiLiterals(s:StringContext) {
  def i():Int = macro Macros.siInt
  def j():Long = macro Macros.siLong
  def big():BigInt = macro Macros.siBigInt
  def dec():BigDecimal = macro Macros.siBigDecimal
}

class UsLiterals(s:StringContext) {
  def i():Int = macro Macros.usInt
  def j():Long = macro Macros.usLong
  def big():BigInt = macro Macros.usBigInt
  def dec():BigDecimal = macro Macros.usBigDecimal
}

class EuLiterals(s:StringContext) {
  def i():Int = macro Macros.euInt
  def j():Long = macro Macros.euLong
  def big():BigInt = macro Macros.euBigInt
  def dec():BigDecimal = macro Macros.euBigDecimal
}

object primitives {
  implicit class IntAs(n:Int) {
    def as[A](implicit ev:Ring[A]):A = macro Macros.intAs[A]
  }

  implicit class DoubleAs(n:Double) {
    def as[A](implicit ev:Field[A]):A = macro Macros.dblAs[A]
  }
}

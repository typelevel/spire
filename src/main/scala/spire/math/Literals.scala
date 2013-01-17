package spire.math

import scala.{specialized => spec}

import spire.macrosk.Macros

class Literals(s:StringContext) {
  def b():Byte = macro Macros.byte
  def h():Short = macro Macros.short

  def ub():UByte = macro Macros.ubyte
  def uh():UShort = macro Macros.ushort
  def ui():UInt = macro Macros.uint
  def ul():ULong = macro Macros.ulong

  def r():Rational = macro Macros.rational
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

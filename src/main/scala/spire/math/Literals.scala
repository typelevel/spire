package spire.math

import language.experimental.macros
import language.implicitConversions

import scala.{specialized => spec}

import spire.macros.Macros

class Literals(s:StringContext) {
  def b():Byte = macro Macros.byte
  def h():Short = macro Macros.short
  def r():Rational = macro Macros.rational
}

object Literals {
  implicit def literals(s:StringContext) = new Literals(s)
}

class Radix(s:StringContext) {
  def x2():Int = macro Macros.radix
  def x8():Int = macro Macros.radix
  def x10():Int = macro Macros.radix
  def x16():Int = macro Macros.radix
}

object Radix {
  implicit def radix(s:StringContext) = new Radix(s)
}

class SiLiterals(s:StringContext) {
  def i():Int = macro Macros.siInt
  def j():Long = macro Macros.siLong
  def big():BigInt = macro Macros.siBigInt
}

object SiLiterals {
  implicit def siLiterals(s:StringContext) = new SiLiterals(s)
}

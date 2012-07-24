package spire.math

import language.experimental.macros

import scala.reflect.makro.Context
import scala.{specialized => spec}

class Literals(s:StringContext) {
  def b():Byte = macro Literals.byte
  def h():Short = macro Literals.short
  def r():Rational = macro Literals.rational
}

object Literals {
  def byte(c:Context)(): c.Expr[Byte] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    val i = s.toInt
    val n:Byte = if (i < -128 || i > 255) {
      throw new ArithmeticException("illegal byte constant: %s" format s)
    } else if (i > 127) {
      (i - 256).toByte
    } else {
      i.toByte
    }
    c.Expr[Byte](Literal(Constant(n)))
  }

  def short(c:Context)(): c.Expr[Short] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    val i = s.toInt
    val n:Short = if (i < -32768 || i > 65535) {
      throw new ArithmeticException("illegal short constant: %s" format s)
    } else if (i > 32767) {
      (i - 65536).toShort
    } else {
      i.toShort
    }
    c.Expr[Short](Literal(Constant(n)))
  }

  def rational(c:Context)(): c.Expr[Rational] = {
    import c.mirror._
    import c.universe._

    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    val mth = Select(Select(Select(Ident("spire"), "math"), "Rational"), "apply")
    val bg = Select(Select(Select(Ident("scala"), "math"), "BigInt"), "apply")

    val r = Rational(s)

    // TODO: might be nice to create "fast" constructors for Rational/BigInt
    // for these kinds of situations (also for serialization/deserialization).

    if (r.numerator <= BigInt(Long.MaxValue) &&
        r.numerator >= BigInt(Long.MinValue) &&
        r.denominator <= BigInt(Long.MaxValue)) {
      val ns = List(r.numerator.toLong, r.denominator.toLong)
      val ts = ns.map(t => Literal(Constant(t)))
      c.Expr[Rational](Apply(mth, ts))
    } else {
      val ns = List(r.numerator.toString, r.denominator.toString)
      val ts = ns.map(t => Apply(bg, List(Literal(Constant(t)))))
      c.Expr[Rational](Apply(mth, ts))
    }
  }
}

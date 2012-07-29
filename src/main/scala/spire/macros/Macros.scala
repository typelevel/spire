package spire.macros

import language.implicitConversions
import language.higherKinds
import language.experimental.macros

import scala.reflect.makro.Context

import spire.math._
import spire.algebra._

object Macros {

  // byte literals
  def byte(c:Context)(): c.Expr[Byte] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    val i = s.toInt
    val n:Byte = if (i < -128 || i > 255) {
      throw new NumberFormatException("illegal byte constant: %s" format s)
    } else if (i > 127) {
      (i - 256).toByte
    } else {
      i.toByte
    }
    c.Expr[Byte](Literal(Constant(n)))
  }

  // short literals
  def short(c:Context)(): c.Expr[Short] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    val i = s.toInt
    val n:Short = if (i < -32768 || i > 65535) {
      throw new NumberFormatException("illegal short constant: %s" format s)
    } else if (i > 32767) {
      (i - 65536).toShort
    } else {
      i.toShort
    }
    c.Expr[Short](Literal(Constant(n)))
  }

  // rational literals

  def bigIntApply(c:Context) = {
    import c.mirror._
    import c.universe._
    Select(Select(Select(Ident("scala"), "math"), "BigInt"), "apply")
  }

  def rationalApply(c:Context) = {
    import c.mirror._
    import c.universe._
    Select(Select(Select(Ident("spire"), "math"), "Rational"), "apply")
  }

  def rational(c:Context)(): c.Expr[Rational] = {
    import c.mirror._
    import c.universe._

    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    val mth = rationalApply(c)
    val bg = bigIntApply(c)
    
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
      val ts = ns.map(t => Apply(bigIntApply(c), List(Literal(Constant(t)))))
      c.Expr[Rational](Apply(mth, ts))
    }
  }

  // short literals
  def isValidSiInt(s:String) = s.matches("0|-?[1-9][0-9]{0,2}( [0-9]{3})*")

  def siInt(c:Context)(): c.Expr[Int] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree

    if (!isValidSiInt(s))
      throw new NumberFormatException("illegal SI Int constant: %s" format s)

    val n = s.replace(" ", "").toInt

    c.Expr[Int](Literal(Constant(n)))
  }

  def siLong(c:Context)(): c.Expr[Long] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree

    if (!isValidSiInt(s))
      throw new NumberFormatException("illegal SI Long constant: %s" format s)

    val n = s.replace(" ", "").toLong

    c.Expr[Long](Literal(Constant(n)))
  }

  def siBigInt(c:Context)(): c.Expr[BigInt] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree

    if (!isValidSiInt(s))
      throw new NumberFormatException("illegal SI BigInt constant: %s" format s)

    val str = s.replace(" ", "")

    c.Expr[BigInt](Apply(bigIntApply(c), List(Literal(Constant(str)))))
  }


  def radix(c:Context)(): c.Expr[Int] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree

    val name = c.macroApplication.symbol.name.toString
    val base = name.substring(1).toInt
    if (base < 2 || 36 < base)
      throw new NumberFormatException("invalid radix: %s" format base)

    val n = java.lang.Integer.parseInt(s, base)

    c.Expr[Int](Literal(Constant(n)))
  }
}

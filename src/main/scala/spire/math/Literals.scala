package spire.math

import language.experimental.macros

import scala.reflect.makro.Context
import scala.{specialized => spec}

class Literals(s:StringContext) {
  def b():Byte = macro Literals.byte
  def s():Short = macro Literals.short
  def r():Rational = macro Literals.rational
}

object Literals {
  def byte(c:Context)(): c.Expr[Byte] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    c.Expr[Byte](Literal(Constant(s.toByte)))
  }

  def short(c:Context)(): c.Expr[Short] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    c.Expr[Short](Literal(Constant(s.toShort)))
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

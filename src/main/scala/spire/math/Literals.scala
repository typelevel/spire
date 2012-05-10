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
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    Expr[Byte](Literal(Constant(s.toByte)))
  }

  def short(c:Context)(): c.Expr[Short] = {
    import c.mirror._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    Expr[Short](Literal(Constant(s.toShort)))
  }

  def rational(c:Context)(): c.Expr[Rational] = {
    import c.mirror._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    try {
      val Array(s1, s2) = s.split("/")
      val n = BigInt(s1)
      val d = BigInt(s2)
      if (n.toLong == n && d.toLong == d) {
        val nn:Long = n.toLong
        val dd:Long = d.toLong
        c.reify(Rational(nn, dd))
      } else {
        c.reify(Rational(n, d))
      }
    }
  }
}

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
package macros

import spire.algebra.{Field, Ring}
import spire.macros.compat.Context
import spire.math.{Rational, UByte, UInt, ULong, UShort}

object Macros {

  case class LiteralUtil(c: Context) {
    import c.universe._

    def getString: String = {
      val Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) = c.prefix.tree: @unchecked
      s
    }
  }

  def parseContext(c: Context, lower: BigInt, upper: BigInt): Either[String, BigInt] =
    parseNumber(LiteralUtil(c).getString, lower, upper)

  def parseNumber(s: String, lower: BigInt, upper: BigInt): Either[String, BigInt] =
    try {
      val n = BigInt(s)
      if (n < lower || n > upper) Left("illegal constant: %s".format(s)) else Right(n)
    } catch {
      case _: Exception => Left("illegal constant: %s".format(s))
    }

  def byte(c: Context)(): c.Expr[Byte] = {
    import c.universe._
    parseContext(c, BigInt(-128), BigInt(255)) match {
      case Right(n) => c.Expr(q"${n.toByte}")
      case Left(s)  => throw new NumberFormatException(s)
    }
  }

  def ubyte(c: Context)(): c.Expr[UByte] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt(255)) match {
      case Right(n) => c.Expr(q"_root_.spire.math.UByte(${n.toByte})")
      case Left(s)  => throw new NumberFormatException(s)
    }
  }

  def short(c: Context)(): c.Expr[Short] = {
    import c.universe._
    parseContext(c, BigInt(-32768), BigInt(65535)) match {
      case Right(n) => c.Expr(q"${n.toShort}")
      case Left(s)  => throw new NumberFormatException(s)
    }
  }

  def ushort(c: Context)(): c.Expr[UShort] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt(65535)) match {
      case Right(n) => c.Expr(q"_root_.spire.math.UShort(${n.toShort})")
      case Left(s)  => throw new NumberFormatException(s)
    }
  }

  def uint(c: Context)(): c.Expr[UInt] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt(4294967295L)) match {
      case Right(n) => c.Expr(q"_root_.spire.math.UInt(${n.toInt})")
      case Left(s)  => throw new NumberFormatException(s)
    }
  }

  def ulong(c: Context)(): c.Expr[ULong] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt("18446744073709551615")) match {
      case Right(n) => c.Expr(q"_root_.spire.math.ULong(${n.toLong})")
      case Left(s)  => throw new NumberFormatException(s)
    }
  }

  def rational(c: Context)(): c.Expr[Rational] = {
    import c.universe._

    val Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) = c.prefix.tree: @unchecked
    val r = Rational(s)

    val (n, d) = (r.numerator, r.denominator)
    if (n.isValidLong && d.isValidLong)
      c.Expr(q"_root_.spire.math.Rational(${n.toLong}, ${d.toLong})")
    else
      c.Expr(q"_root_.spire.math.Rational(BigInt(${n.toString}), BigInt(${d.toString}))")
  }

  def formatWhole(c: Context, sep: String): String = {
    val esep = if (sep == ".") "\\." else sep
    val regex = "(0|-?[1-9][0-9]{0,2}(%s[0-9]{3})*)".format(esep)
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) = c.prefix.tree: @unchecked
    if (!s.matches(regex)) c.error(c.enclosingPosition, "invalid whole number")
    s.replace(sep, "")
  }

  def formatDecimal(c: Context, sep: String, dec: String): String = {
    val esep = if (sep == ".") "\\." else sep
    val edec = if (dec == ".") "\\." else dec
    val regex = "-?(0|[1-9][0-9]{0,2}(%s[0-9]{3})*)(%s[0-9]+)?".format(esep, edec)
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) = c.prefix.tree: @unchecked
    if (!s.matches(regex)) c.error(c.enclosingPosition, "invalid decimal number")
    s.replace(sep, "").replace(dec, ".")
  }

  def handleInt(c: Context, name: String, sep: String): c.Expr[Int] = {
    import c.universe._
    try {
      c.Expr[Int](Literal(Constant(formatWhole(c, sep).toInt)))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s Int constant".format(name))
    }
  }

  def handleLong(c: Context, name: String, sep: String): c.Expr[Long] = {
    import c.universe._
    try {
      c.Expr[Long](Literal(Constant(formatWhole(c, sep).toLong)))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s Long constant".format(name))
    }
  }

  def handleBigInt(c: Context, name: String, sep: String): c.Expr[BigInt] = {
    import c.universe._
    try {
      val s = formatWhole(c, sep)
      BigInt(s) // make sure it's ok
      c.Expr[BigInt](Apply(q"_root_.scala.math.BigInt.apply", List(Literal(Constant(s)))))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s BigInt constant".format(name))
    }
  }

  def handleBigDecimal(c: Context, name: String, sep: String, dec: String): c.Expr[BigDecimal] = {
    import c.universe._
    try {
      val s = formatDecimal(c, sep, dec)
      BigDecimal(s) // make sure it's ok
      c.Expr[BigDecimal](Apply(q"_root_.scala.math.BigDecimal.apply", List(Literal(Constant(s)))))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s BigDecimal constant".format(name))
    }
  }

  def siInt(c: Context)(): c.Expr[Int] = handleInt(c, "SI", " ")
  def siLong(c: Context)(): c.Expr[Long] = handleLong(c, "SI", " ")
  def siBigInt(c: Context)(): c.Expr[BigInt] = handleBigInt(c, "SI", " ")
  def siBigDecimal(c: Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "SI", " ", "\\.")

  def usInt(c: Context)(): c.Expr[Int] = handleInt(c, "US", ",")
  def usLong(c: Context)(): c.Expr[Long] = handleLong(c, "US", ",")
  def usBigInt(c: Context)(): c.Expr[BigInt] = handleBigInt(c, "US", ",")
  def usBigDecimal(c: Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "US", ",", "\\.")

  def euInt(c: Context)(): c.Expr[Int] = handleInt(c, "EU", ".")
  def euLong(c: Context)(): c.Expr[Long] = handleLong(c, "EU", ".")
  def euBigInt(c: Context)(): c.Expr[BigInt] = handleBigInt(c, "EU", ".")
  def euBigDecimal(c: Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "EU", ".", ",")

  def radix(c: Context)(): c.Expr[Int] = {
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) = c.prefix.tree: @unchecked

    val name = c.macroApplication.symbol.name.toString
    val base = name.substring(1).toInt
    if (base < 2 || 36 < base)
      throw new NumberFormatException("invalid radix: %s".format(base))

    val n = java.lang.Integer.parseInt(s, base)

    c.Expr[Int](Literal(Constant(n)))
  }

  def intAs[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Ring[A]]): c.Expr[A] = {
    import c.universe._
    c.Expr[A]((c.prefix.tree: @unchecked) match {
      case Apply(_, List(Literal(Constant(0)))) => q"$ev.zero"
      case Apply(_, List(Literal(Constant(1)))) => q"$ev.one"
      case Apply(_, List(n))                    => q"$ev.fromInt($n)"
    })
  }

  def dblAs[A: c.WeakTypeTag](c: Context)(ev: c.Expr[Field[A]]): c.Expr[A] = {
    import c.universe._
    c.Expr[A]((c.prefix.tree: @unchecked) match {
      case Apply(_, List(Literal(Constant(0.0)))) => q"$ev.zero"
      case Apply(_, List(Literal(Constant(1.0)))) => q"$ev.one"
      case Apply(_, List(n))                      => q"$ev.fromDouble($n)"
    })
  }
}

package spire.macros

import spire.macros.compat.Context
import spire.math.{Rational, UByte, UShort, UInt, ULong}

object Macros {

  case class LiteralUtil(c: Context) {
    import c.universe._

    def getString: String = {
      val Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) = c.prefix.tree
      s
    }
  }

  def parseContext(c: Context, lower: BigInt, upper: BigInt): Either[String, BigInt] =
    parseNumber(LiteralUtil(c).getString, lower, upper)

  def parseNumber(s: String, lower: BigInt, upper: BigInt): Either[String, BigInt] =
    try {
      val n = BigInt(s)
      if (n < lower || n > upper) Left("illegal constant: %s" format s) else Right(n)
    } catch {
      case _: Exception => Left("illegal constant: %s" format s)
    }

  def byte(c: Context)(): c.Expr[Byte] = {
    import c.universe._
    parseContext(c, BigInt(-128), BigInt(255)) match {
      case Right(n) => c.Expr(q"${n.toByte}")
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  def ubyte(c: Context)(): c.Expr[UByte] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt(255)) match {
      case Right(n) => c.Expr(q"spire.math.UByte(${n.toByte})")
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  def short(c: Context)(): c.Expr[Short] = {
    import c.universe._
    parseContext(c, BigInt(-32768), BigInt(65535)) match {
      case Right(n) => c.Expr(q"${n.toShort}")
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  def ushort(c: Context)(): c.Expr[UShort] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt(65535)) match {
      case Right(n) => c.Expr(q"spire.math.UShort(${n.toShort})")
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  def uint(c: Context)(): c.Expr[UInt] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt(4294967295L)) match {
      case Right(n) => c.Expr(q"spire.math.UInt(${n.toInt})")
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  def ulong(c: Context)(): c.Expr[ULong] = {
    import c.universe._
    parseContext(c, BigInt(0), BigInt("18446744073709551615")) match {
      case Right(n) => c.Expr(q"spire.math.ULong(${n.toLong})")
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  def rational(c: Context)(): c.Expr[Rational] = {
    import c.universe._

    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    val r = Rational(s)

    val (n, d) = (r.numerator, r.denominator)
    if (n.isValidLong && d.isValidLong)
      c.Expr(q"spire.math.Rational(${n.toLong}, ${d.toLong})")
    else
      c.Expr(q"spire.math.Rational(BigInt(${n.toString}), BigInt(${d.toString}))")
  }

  def formatWhole(c: Context, sep: String): String = {
    val regex = "0|-?[1-9][0-9]{0,2}(%s[0-9]{3})*" format sep
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    if (!s.matches(regex)) c.error(c.enclosingPosition, "invalid whole number")
    s.replace(sep, "")
  }

  def formatDecimal(c: Context, sep: String, dec: String): String = {
    val regex = "0|-?[1-9][0-9]{0,2}(%s[0-9]{3})*(%s[0-9]+)?" format (sep, dec)
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    if (!s.matches(regex)) c.error(c.enclosingPosition, "invalid decimal number")
    s.replace(sep, "").replace(dec, ".")
  }

  def handleInt(c: Context, name: String, sep: String): c.Expr[Int] = {
    import c.universe._
    try {
      c.Expr[Int](Literal(Constant(formatWhole(c, sep).toInt)))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s Int constant" format name)
    }
  }

  def handleLong(c: Context, name: String, sep: String): c.Expr[Long] = {
    import c.universe._
    try {
      c.Expr[Long](Literal(Constant(formatWhole(c, sep).toLong)))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s Long constant" format name)
    }
  }

  def handleBigInt(c: Context, name: String, sep: String): c.Expr[BigInt] = {
    import c.universe._
    try {
      val s = formatWhole(c, sep)
      val b = BigInt(s) // make sure it's ok
      c.Expr[BigInt](Apply(q"scala.math.BigInt.apply", List(Literal(Constant(s)))))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s BigInt constant" format name)
    }
  }

  def handleBigDecimal(c: Context, name: String, sep: String, dec: String): c.Expr[BigDecimal] = {
    import c.universe._
    try {
      val s = formatDecimal(c, sep, dec)
      val b = BigDecimal(s) // make sure it's ok
      c.Expr[BigDecimal](Apply(q"scala.math.BigDecimal.apply", List(Literal(Constant(s)))))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s BigInt constant" format name)
    }
  }

  def siInt(c: Context)(): c.Expr[Int] = handleInt(c, "SI", " ")
  def siLong(c: Context)(): c.Expr[Long] = handleLong(c, "SI", " ")
  def siBigInt(c: Context)(): c.Expr[BigInt] = handleBigInt(c, "SI", " ")
  def siBigDecimal(c: Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "SI", " ", ".")

  def usInt(c: Context)(): c.Expr[Int] = handleInt(c, "US", ",")
  def usLong(c: Context)(): c.Expr[Long] = handleLong(c, "US", ",")
  def usBigInt(c: Context)(): c.Expr[BigInt] = handleBigInt(c, "US", ",")
  def usBigDecimal(c: Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "US", ",", ".")

  def euInt(c: Context)(): c.Expr[Int] = handleInt(c, "EU", ".")
  def euLong(c: Context)(): c.Expr[Long] = handleLong(c, "EU", ".")
  def euBigInt(c: Context)(): c.Expr[BigInt] = handleBigInt(c, "EU", ".")
  def euBigDecimal(c: Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "EU", ".", ",")

  def radix(c: Context)(): c.Expr[Int] = {
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

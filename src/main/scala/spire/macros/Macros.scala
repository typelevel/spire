package spire.macrosk

import language.implicitConversions
import language.higherKinds
import language.experimental.macros

import scala.reflect.macros.Context

import spire.math._
import spire.algebra._

object Macros {
  def parseTree1(c:Context): String = {
    import c.mirror._
    import c.universe._
    c.prefix.tree match {
      case Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) => s
    }
  }

  def parseByte(s: String): Either[String, Byte] = try {
    val i = s.toInt
    if (i < -128 || i > 255) {
      Left("illegal byte constant: %s" format s)
    } else if (i > 127) {
      Right((i - 256).toByte)
    } else {
      Right(i.toByte)
    }
  } catch {
    case _: Exception => Left("illegal byte constant: %s" format s)
  }

  // byte literals
  def byte(c:Context)(): c.Expr[Byte] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseByte(s) match {
      case Right(n) => c.Expr[Byte](Literal(Constant(n)))
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  def parseShort(s: String): Either[String, Short] = try {
    val i = s.toInt
    if (i < -32768 || i > 65535) {
      Left("illegal short constant: %s" format s)
    } else if (i > 32767) {
      Right((i - 65536).toShort)
    } else {
      Right(i.toShort)
    }
  } catch {
    case _: Exception => Left("illegal short constant: %s" format s)
  }

  // short literals
  def short(c:Context)(): c.Expr[Short] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseShort(s) match {
      case Right(n) => c.Expr[Short](Literal(Constant(n)))
      case Left(s) => throw new NumberFormatException(s)
    }
  }

  // rational literals

  def bigIntApply(c:Context) = {
    import c.mirror._
    import c.universe._
    Select(Select(Select(Ident("scala"), "math"), "BigInt"), "apply")
  }

  def bigDecimalApply(c:Context) = {
    import c.mirror._
    import c.universe._
    Select(Select(Select(Ident("scala"), "math"), "BigDecimal"), "apply")
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

  def formatWhole(c: Context, sep: String): String = {
    val regex = "0|-?[1-9][0-9]{0,2}(%s[0-9]{3})*" format sep
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    if (!s.matches(regex)) sys.error("invalid")
    s.replace(sep, "")
  }

  def formatDecimal(c: Context, sep: String, dec: String): String = {
    val regex = "0|-?[1-9][0-9]{0,2}(%s[0-9]{3})*(%s[0-9]+)?" format (sep, dec)
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    if (!s.matches(regex)) sys.error("invalid")
    s.replace(sep, "").replace(dec, ".")
  }

  def handleInt(c: Context, name: String, sep: String): c.Expr[Int] = {
    import c.mirror._
    import c.universe._
    try {
      c.Expr[Int](Literal(Constant(formatWhole(c, sep).toInt)))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s Int constant" format name)
    }
  }

  def handleLong(c: Context, name: String, sep: String): c.Expr[Long] = {
    import c.mirror._
    import c.universe._
    try {
      c.Expr[Long](Literal(Constant(formatWhole(c, sep).toLong)))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s Long constant" format name)
    }
  }

  def handleBigInt(c:Context, name: String, sep: String): c.Expr[BigInt] = {
    import c.mirror._
    import c.universe._
    try {
      val s = formatWhole(c, sep)
      val b = BigInt(s) // make sure it's ok
      c.Expr[BigInt](Apply(bigIntApply(c), List(Literal(Constant(s)))))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s BigInt constant" format name)
    }
  }

  def handleBigDecimal(c:Context, name: String, sep: String, dec: String): c.Expr[BigDecimal] = {
    import c.mirror._
    import c.universe._
    try {
      val s = formatDecimal(c, sep, dec)
      val b = BigDecimal(s) // make sure it's ok
      c.Expr[BigDecimal](Apply(bigDecimalApply(c), List(Literal(Constant(s)))))
    } catch {
      case e: Exception =>
        throw new NumberFormatException("illegal %s BigInt constant" format name)
    }
  }

  def siInt(c:Context)(): c.Expr[Int] = handleInt(c, "SI", " ")
  def siLong(c:Context)(): c.Expr[Long] = handleLong(c, "SI", " ")
  def siBigInt(c:Context)(): c.Expr[BigInt] = handleBigInt(c, "SI", " ")
  def siBigDecimal(c:Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "SI", " ", ".")

  def usInt(c:Context)(): c.Expr[Int] = handleInt(c, "US", ",")
  def usLong(c:Context)(): c.Expr[Long] = handleLong(c, "US", ",")
  def usBigInt(c:Context)(): c.Expr[BigInt] = handleBigInt(c, "US", ",")
  def usBigDecimal(c:Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "US", ",", ".")

  def euInt(c:Context)(): c.Expr[Int] = handleInt(c, "EU", ".")
  def euLong(c:Context)(): c.Expr[Long] = handleLong(c, "EU", ".")
  def euBigInt(c:Context)(): c.Expr[BigInt] = handleBigInt(c, "EU", ".")
  def euBigDecimal(c:Context)(): c.Expr[BigDecimal] = handleBigDecimal(c, "EU", ".", ",")

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

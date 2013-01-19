package spire.macrosk

import scala.reflect.macros.Context

import spire.math._
import spire.algebra._

object Macros {
  def parseContext(c: Context, lower: BigInt, upper: BigInt): Either[String, BigInt] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseNumber(s, lower, upper)
  }

  def parseNumber(s: String, lower: BigInt, upper: BigInt): Either[String, BigInt] = {
    try {
      val n = BigInt(s)
      if (n < lower || n > upper) Left("illegal constant: %s" format s) else Right(n)
    } catch {
      case _: Exception => Left("illegal constant: %s" format s)
    }
  }

  def byte(c:Context)(): c.Expr[Byte] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseContext(c, BigInt(-128), BigInt(255)) match {
      case Right(n) => c.Expr[Byte](Literal(Constant(n.toByte)))
      case Left(s) => throw new NumberFormatException(s)
    }
  }
  def ubyte(c:Context)(): c.Expr[UByte] = {
    import c.mirror._
    import c.universe._
    parseContext(c, BigInt(0), BigInt(255)) match {
      case Right(n) =>
        val e = c.Expr[Byte](Literal(Constant(n.toByte)))
        reify { spire.math.UByte(e.splice) }
      case Left(s) =>
        throw new NumberFormatException(s)
    }
  }

  def short(c:Context)(): c.Expr[Short] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseContext(c, BigInt(-32768), BigInt(65535)) match {
      case Right(n) => c.Expr[Short](Literal(Constant(n.toShort)))
      case Left(s) => throw new NumberFormatException(s)
    }
  }
  def ushort(c:Context)(): c.Expr[UShort] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseContext(c, BigInt(0), BigInt(65535)) match {
      case Right(n) =>
        val e = c.Expr[Short](Literal(Constant(n.toShort)))
        reify { spire.math.UShort(e.splice) }
      case Left(s) =>
        throw new NumberFormatException(s)
    }
  }

  def uint(c:Context)(): c.Expr[UInt] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseContext(c, BigInt(0), BigInt(4294967295L)) match {
      case Right(n) =>
        val e = c.Expr[Int](Literal(Constant(n.toInt)))
        reify { spire.math.UInt(e.splice) }
      case Left(s) =>
        throw new NumberFormatException(s)
    }
  }

  def ulong(c:Context)(): c.Expr[ULong] = {
    import c.mirror._
    import c.universe._
    val Apply(_, List(Apply(_, List(Literal(Constant(s:String)))))) = c.prefix.tree
    parseContext(c, BigInt(0), BigInt("18446744073709551615")) match {
      case Right(n) =>
        val e = c.Expr[Long](Literal(Constant(n.toLong)))
        reify { spire.math.ULong(e.splice) }
      case Left(s) =>
        throw new NumberFormatException(s)
    }
  }

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

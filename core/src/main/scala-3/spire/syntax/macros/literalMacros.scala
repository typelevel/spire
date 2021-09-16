
package spire.syntax.macros

import quoted._

import spire.math.*

def parseNumber(s: Seq[String], lower: BigInt, upper: BigInt): Either[String, BigInt] =
  s.headOption.map { s =>
    try
      val n = BigInt(s)
      if (n < lower || n > upper) Left(s"illegal constant: $s") else Right(n)
    catch
      case _: Exception => Left(s"illegal constant: %s")
  }.getOrElse(Left("Unsupported parcialized strings"))


def byte(digits: Expr[StringContext])(using Quotes): Expr[Byte] =
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(-128), BigInt(255)) match
    case Right(a) => Expr(a.toByte)
    case Left(b) =>
      report.error(b)
      '{0.toByte}

def short(digits: Expr[StringContext])(using Quotes): Expr[Short] =
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(-32768), BigInt(65535)) match
    case Right(a) => Expr(a.toShort)
    case Left(b) =>
      report.error(b)
      '{0.toShort}

def ubyte(digits: Expr[StringContext])(using Quotes): Expr[UByte] =
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt(255)) match
    case Right(a) => '{UByte(${Expr(a.toByte)})}
    case Left(b) =>
      report.error(b)
      '{UByte(0)}

def ushort(digits: Expr[StringContext])(using Quotes): Expr[UShort] =
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt(65535)) match
    case Right(a) => '{UShort(${Expr(a.toShort)})}
    case Left(b) =>
      report.error(b)
      '{UShort(0)}

def uint(digits: Expr[StringContext])(using Quotes): Expr[UInt] =
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt(4294967295L)) match
    case Right(a) => '{UInt(${Expr(a.toInt)})}
    case Left(b) =>
      report.error(b)
      '{UInt(0)}

def ulong(digits: Expr[StringContext])(using Quotes): Expr[ULong] =
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt("18446744073709551615")) match
    case Right(a) => '{ULong(${Expr(a.toLong)})}
    case Left(b) =>
      report.error(b)
      '{ULong(0)}

def rational(digits: Expr[StringContext])(using Quotes): Expr[Rational] =
  import quotes.reflect._

  digits.valueOrError.parts.headOption.map { s =>
    val r = Rational(s)
    val (n, d) = (r.numerator, r.denominator)
    if (n.isValidLong && d.isValidLong)
      '{Rational(${Expr(n.toLong)}, ${Expr(d.toLong)})}
    else
      '{Rational(BigInt(${Expr(n.toString)}), BigInt(${Expr(d.toLong)}))}
  }.getOrElse {
    report.error("Not a valid rational")
    '{Rational(0)}
  }

def formatWhole(s: String, sep: String)(using Quotes): String =
  import quotes.reflect._
  val esep = if (sep == ".") "\\." else sep
  val regex = "(0|-?[1-9][0-9]{0,2}(%s[0-9]{3})*)".format(esep)
  if (!s.matches(regex)) report.error("invalid whole number")
  s.replace(sep, "")

def formatDecimal(s: String, sep: String, dec: String)(using Quotes): String =
  import quotes.reflect._
  val esep = if (sep == ".") "\\." else sep
  val edec = if (dec == ".") "\\." else dec
  val regex = "-?(0|[1-9][0-9]{0,2}(%s[0-9]{3})*)(%s[0-9]+)?".format(esep, edec)
  if (!s.matches(regex)) report.error("invalid whole number")
  s.replace(sep, "").replace(dec, ".")

def handleInt(s: Seq[String], name: String, sep: String)(using Quotes): Expr[Int] =
  import quotes.reflect._
  s.headOption.map { s =>
    try
      Expr(formatWhole(s, sep).toInt)
    catch
      case e: Exception =>
        throw new NumberFormatException("illegal %s Int constant".format(name))
  }.getOrElse {
    report.error("Unsupported parcialized strings")
    '{0}
  }

def handleLong(s: Seq[String], name: String, sep: String)(using Quotes): Expr[Long] =
  import quotes.reflect._
  s.headOption.map { s =>
    try
      Expr(formatWhole(s, sep).toLong)
    catch
      case e: Exception =>
        throw new NumberFormatException("illegal %s Long constant".format(name))
  }.getOrElse {
    report.error("Unsupported parcialized strings")
    '{0}
  }

def handleBigInt(s: Seq[String], name: String, sep: String)(using Quotes): Expr[BigInt] =
  import quotes.reflect._
  s.headOption.map { s =>
    try
      val r = formatWhole(s, sep)
      BigInt(r) // make sure it's ok
      '{BigInt(${Expr(r)})}
    catch
      case e: Exception =>
        throw new NumberFormatException("illegal %s BigInt constant".format(name))
  }.getOrElse {
    report.error("Unsupported parcialized strings")
    '{BigInt(0)}
  }

def handleBigDecimal(s: Seq[String], name: String, sep: String, dec: String)(using Quotes): Expr[BigDecimal] =
  import quotes.reflect._
  s.headOption.map { s =>
    try
      val r = formatDecimal(s, sep, dec)
      BigDecimal(r) // make sure it's ok
      '{BigDecimal(${Expr(r)})}
    catch
      case e: Exception =>
        throw new NumberFormatException(s"illegal $name BigDecimal constant")
  }.getOrElse {
    report.error("Unsupported parcialized strings")
    '{BigDecimal(0)}
  }

def siInt(digits: Expr[StringContext])(using Quotes): Expr[Int] =
  handleInt(digits.valueOrError.parts, "SI", " ")

def siLong(digits: Expr[StringContext])(using Quotes): Expr[Long] =
  handleLong(digits.valueOrError.parts, "SI", " ")

def siBigInt(digits: Expr[StringContext])(using Quotes): Expr[BigInt] =
  handleBigInt(digits.valueOrError.parts, "SI", " ")

def siBigDecimal(digits: Expr[StringContext])(using Quotes): Expr[BigDecimal] =
  handleBigDecimal(digits.valueOrError.parts, "SI", " ", "\\.")

def usInt(digits: Expr[StringContext])(using Quotes): Expr[Int] =
  handleInt(digits.valueOrError.parts, "US", ",")

def usLong(digits: Expr[StringContext])(using Quotes): Expr[Long] =
  handleLong(digits.valueOrError.parts, "US", ",")

def usBigInt(digits: Expr[StringContext])(using Quotes): Expr[BigInt] =
  handleBigInt(digits.valueOrError.parts, "US", ",")

def usBigDecimal(digits: Expr[StringContext])(using Quotes): Expr[BigDecimal] =
  handleBigDecimal(digits.valueOrError.parts, "US", ",", "\\.")

def euInt(digits: Expr[StringContext])(using Quotes): Expr[Int] =
  handleInt(digits.valueOrError.parts, "EU", ".")

def euLong(digits: Expr[StringContext])(using Quotes): Expr[Long] =
  handleLong(digits.valueOrError.parts, "EU", ".")

def euBigInt(digits: Expr[StringContext])(using Quotes): Expr[BigInt] =
  handleBigInt(digits.valueOrError.parts, "EU", ".")

def euBigDecimal(digits: Expr[StringContext])(using Quotes): Expr[BigDecimal] =
  handleBigDecimal(digits.valueOrError.parts, "EU", ".", ",")

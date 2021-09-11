
package spire.syntax.macros

import quoted._

import spire.algebra.{Field, CRing}
import spire.math.UByte
import spire.math.UShort
import spire.math.UInt
import spire.math.ULong

def fromRingImpl[A: Type](digits: Expr[String], radix: Expr[Int], A: Expr[CRing[A]])(using quotes: Quotes): Expr[A] =
  import quotes._
  import quotes.reflect._

  ((digits -> radix): @unchecked) match
    case Literal(StringConstant(ds)) -> Literal(IntConstant(r)) => fromBigIntImpl(BigInt(ds, r), A)
    case _                     => '{ $A.fromBigInt(BigInt($digits, $radix)) }

def fromFieldImpl[A: Type](digits: Expr[String], A: Expr[Field[A]])(using quatos: Quotes): Expr[A] =
  import quotes._
  import quotes.reflect._
  digits match
    case Literal(StringConstant(ds)) =>
      if floating.matches(ds) then
        val bigdec = BigDecimal(ds)
        if bigdec.isDecimalDouble || bigdec.isBinaryDouble || bigdec.isExactDouble then bigdec.toDouble match
          case 0.0 => '{ $A.zero                     }
          case 1.0 => '{ $A.one                      }
          case n   => '{ $A.fromDouble(${ Expr(n) }) }
        else
          '{ $A.zero                     }
          // '{ $A.fromBigDecimal(${ Expr(bigdec) }) }
      else
        fromBigIntImpl(BigInt(ds), A)

    // case _ => '{ $A.fromBigDecimal(BigDecimal($digits)) }

private def fromBigIntImpl[A: Type](bigint: BigInt, A: Expr[CRing[A]])(using Quotes): Expr[A] =
  if bigint.isValidInt then bigint.toInt match
    case 0 => '{ $A.zero                  }
    case 1 => '{ $A.one                   }
    case n => '{ $A.fromInt(${ Expr(n) }) }
  else '{ $A.fromBigInt(${ Expr(bigint) }) }

private val floating = """.*[.eE].*""".r

// private case class LiteralUtil(c: Context) {
//
//   def getString: String = {
//     val Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) = c.prefix.tree: @unchecked
//     s
//   }
// }

def parseNumber(s: Seq[String], lower: BigInt, upper: BigInt): Either[String, BigInt] =
  s.headOption.map { s =>
    try {
      val n = BigInt(s)
      if (n < lower || n > upper) Left(s"illegal constant: $s") else Right(n)
    } catch {
      case _: Exception => Left(s"illegal constant: %s")
    }
  }.getOrElse(Left("Unsupported parcialized strings"))


def byte(digits: Expr[StringContext])(using Quotes): Expr[Byte] =
  import quotes._
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(-128), BigInt(255)) match
    case Right(a) => Expr(a.toByte)
    case Left(b) =>
      report.info(b)
      '{0.toByte}

def short(digits: Expr[StringContext])(using Quotes): Expr[Short] =
  import quotes._
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(-32768), BigInt(65535)) match
    case Right(a) => Expr(a.toShort)
    case Left(b) =>
      report.info(b)
      '{0.toShort}

def ubyte(digits: Expr[StringContext])(using Quotes): Expr[UByte] =
  import quotes._
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt(255)) match
    case Right(a) => '{UByte(${Expr(a.toByte)})}
    case Left(b) =>
      report.info(b)
      '{UByte(0)}

def ushort(digits: Expr[StringContext])(using Quotes): Expr[UShort] =
  import quotes._
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt(65535)) match
    case Right(a) => '{UShort(${Expr(a.toShort)})}
    case Left(b) =>
      report.info(b)
      '{UShort(0)}

def uint(digits: Expr[StringContext])(using Quotes): Expr[UInt] =
  import quotes._
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt(4294967295L)) match
    case Right(a) => '{UInt(${Expr(a.toInt)})}
    case Left(b) =>
      report.info(b)
      '{UInt(0)}

def ulong(digits: Expr[StringContext])(using Quotes): Expr[ULong] =
  import quotes._
  import quotes.reflect._

  parseNumber(digits.valueOrError.parts, BigInt(0), BigInt("18446744073709551615")) match
    case Right(a) => '{ULong(${Expr(a.toLong)})}
    case Left(b) =>
      report.info(b)
      '{ULong(0)}

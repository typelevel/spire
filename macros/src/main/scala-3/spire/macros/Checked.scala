package spire
package macros

import scala.language.existentials
import scala.quoted.*

class ArithmeticOverflowException() extends ArithmeticException("arithmetic overflow detected")

object Checked:

  /**
   * Performs overflow checking for Int/Long operations.
   *
   * If no errors are detected, the expected result will be
   * returned. If an error is detected, an ArithmeticOverflowException
   * will be thrown.
   */
  inline def checked[A](inline n: A): A =
    ${ checkedImpl[A]('{n}, '{throw new spire.macros.ArithmeticOverflowException()}) }

  // Attempts to convert the expresion to Int
  private def toInt[A](n: Expr[A])(using Quotes, Type[A]): Expr[Int] =
    import quotes.reflect.*
    if (n.isExprOf[Int])
      n.asExprOf[Int]
    else if (n.isExprOf[Byte])
      '{${n.asExprOf[Byte]}.toInt}
    else if (n.isExprOf[Short])
      '{${n.asExprOf[Short]}.toInt}
    else
       report.error(s"Cannot lift value to int type ${Expr.betaReduce(n).show}")
       '{${n.asExprOf[Long]}.intValue}

  // Attempts to convert the expresion to Long
  private def toLong[A](n: Expr[A])(using Quotes, Type[A]): Expr[Long] =
    import quotes.reflect.*
    if (n.isExprOf[Int])
      '{${n.asExprOf[Int]}.toLong}
    else if (n.isExprOf[Byte])
      '{${n.asExprOf[Byte]}.toLong}
    else if (n.isExprOf[Short])
      '{${n.asExprOf[Short]}.toLong}
    else if (n.isExprOf[Long])
      n.asExprOf[Long]
    else
      report.error(s"Cannot lift value ${n.show} to long type")
      '{${n.asExprOf[Long]}.longValue}

  // Determines if the expression is int like
  private def isIntType[A](n: Expr[A])(using Quotes, Type[A]): Boolean =
    n.isExprOf[Int] || n.isExprOf[Byte] || n.isExprOf[Short]

  // Build an expression with the correct limit for Int/Long
  private def isLongType[A](n: Expr[A])(using Quotes, Type[A]): Boolean =
    n.isExprOf[Long]

  private def limit[A](n: Expr[A])(using Quotes, Type[A]): Expr[Long] =
    if (isIntType[A](n))
      '{Int.MinValue.toLong} // toLong avoids boxing
    else
      '{Long.MinValue}

  private def checkedImpl[A](n: Expr[A], fallback: Expr[Any])(using Quotes, Type[A]): Expr[A] =
    import quotes.reflect.*

    val tree: Term = n.asTerm
    val numLimit = limit[A](n)

    val acc = new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case Select(x, "unary_-") =>
            val isInt = isIntType(x.asExpr)
            val isLong = isLongType(x.asExpr)
            if (isInt)
              '{
                  val z = ${toInt(checkedImpl(x.asExprOf[Any], fallback))}
                  if (z == ${numLimit}) $fallback else -z
              }.asExprOf[A].asTerm
            else if (isLong)
              '{
                  val z = ${toLong(checkedImpl(x.asExprOf[Any], fallback))}
                  if (z == ${numLimit}) $fallback else -z
              }.asTerm
            else super.transformTerm(tree)(owner)
            // NOTE I couldn't find a way to unify the long and int branches. Suggestions are welcome
          case Apply(Select(x, "*"), List(y)) =>
            val isInt = isIntType(x.asExpr) && isIntType(y.asExpr)
            val isLong = isLongType(x.asExpr) || isLongType(y.asExpr)
            if (isInt) {
            '{
              val xt = ${toInt(checkedImpl(x.asExprOf[Any], fallback))}
              val yt = ${toInt(checkedImpl(y.asExprOf[Any], fallback))}
              val z = xt * yt
              if (xt == 0  || (yt == z / xt && !(xt == -1 && yt == $numLimit))) z else $fallback
            }.asTerm
            } else if (isLong) {
            '{
              val xt = ${toLong(checkedImpl(x.asExprOf[Any], fallback))}
              val yt = ${toLong(checkedImpl(y.asExprOf[Any], fallback))}
              val z = xt * yt
              if (xt == 0  || (yt == z / xt && !(xt == -1 && yt == $numLimit))) z else $fallback
            }.asTerm
            } else
            super.transformTerm(tree)(owner)
          case Apply(Select(x, "+"), List(y)) =>
            val isInt = isIntType(x.asExpr) && isIntType(y.asExpr)
            val isLong = isLongType(x.asExpr) || isLongType(y.asExpr)
            if (isInt)
              '{
                val xt = ${toInt(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toInt(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt + yt
                if ((~(xt ^ yt) & (xt ^ z)) < 0) $fallback else z
              }.asTerm
            else if (isLong)
              '{
                val xt = ${toLong(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toLong(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt + yt
                if ((~(xt ^ yt) & (xt ^ z)) < 0) $fallback else z
              }.asTerm
            else super.transformTerm(tree)(owner)
          case Apply(Select(x, "-"), List(y)) =>
            val isInt = isIntType(x.asExpr) && isIntType(y.asExpr)
            val isLong = isLongType(x.asExpr) || isLongType(y.asExpr)
            if (isInt)
              '{
                val xt = ${toInt(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toInt(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt - yt
                if (((xt ^ yt) & (xt ^ z)) < 0) $fallback else z
              }.asTerm
            else if (isLong)
              '{
                val xt = ${toLong(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toLong(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt - yt
                if (((xt ^ yt) & (xt ^ z)) < 0) $fallback else z
              }.asTerm
            else super.transformTerm(tree)(owner)
          case Apply(Select(x, "/"), List(y)) =>
            val isInt = isIntType(x.asExpr) && isIntType(y.asExpr)
            val isLong = isLongType(x.asExpr) || isLongType(y.asExpr)
            if (isInt)
              '{
                val xt = ${toInt(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toInt(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt / yt
                if (yt == -1 && xt == $numLimit) $fallback else z
              }.asTerm
            else if (isLong)
              '{
                val xt = ${toLong(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toLong(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt / yt
                if (yt == -1 && xt == $numLimit) $fallback else z
              }.asTerm
            else super.transformTerm(tree)(owner)
          case _ =>
            super.transformTerm(tree)(owner)

    val result = acc.transformTerm(tree)(tree.symbol).asExprOf[A]
    // report.info(result.show)
    result

  /**
   * Performs overflow checking for Int/Long operations.
 *
   * If no errors are detected, the expected result will be returned
   * in a Some wrapper. If an error is detected, None will be
   * returned.
   */
  inline def option[A](inline n: A): Option[A] =
    try
      Some(checked(n))
    catch
      case a: ArithmeticOverflowException => None

  /**
   * Performs overflow checking for Int/Long operations.
   *
   * If no errors are detected, the expected result will be
   * returned. If there are errors, the 'orElse' block will be
   * evaluated and returned.
   */
  inline def tryOrElse[A](inline n: A)(orElse: => A): A =
    try
      checked(n)
    catch
      case a: ArithmeticOverflowException => orElse

end Checked


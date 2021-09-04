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
   * returned. If there are errors, the 'orElse' block will be
   * evaluated and returned.
   */
  inline def tryOrElse[A](n: Int)(orElse: Int): Int = tryOrReturn(n)(orElse)
  inline def tryOrElse[A](n: Long)(orElse: Long): Long = tryOrReturn(n)(orElse)
  inline def tryOrElse[A](n: A)(orElse: A): A = tryOrReturn(n)(orElse)



  /**
   * Performs overflow checking for Int/Long operations.
   *
   * If no errors are detected, the expected result will be
   * returned. If an error is detected, an ArithmeticOverflowException
   * will be thrown.
   */
  // NOTE I made three versions for each type to know that checkedImpl cannot be called with an arbitrary type
  inline def checked(inline n: Int): Int =
    ${ checkedImpl[Int]('{n}, '{throw new spire.macros.ArithmeticOverflowException()}) }
  inline def checked(inline n: Long): Long =
    ${ checkedImpl[Long]('{n}, '{throw new spire.macros.ArithmeticOverflowException()}) }
  inline def checked[A](inline n: A): A = n

  // Attempts to convert the expresion to Int
  private def toInt[A](n: Expr[A])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    if (n.isExprOf[Int])
      n.asExprOf[Int]
    else if (n.isExprOf[Byte])
      '{${n.asExprOf[Byte]}.toInt}
    else if (n.isExprOf[Short])
      '{${n.asExprOf[Short]}.toInt}
    else
       report.error("Cannot lift value to int type")
       '{${n.asExprOf[Long]}.intValue}

  // Attempts to convert the expresion to Long
  private def toLong[A](n: Expr[A])(using Quotes): Expr[Long] =
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
  private def isIntType[A](n: Expr[A])(using Quotes): Boolean =
    n.isExprOf[Int] || n.isExprOf[Byte] || n.isExprOf[Short]

  // Build an expression with the correct limit for Int/Long
  private def isLongType[A](n: Expr[A])(using Quotes): Boolean =
    n.isExprOf[Long]

  private def limit[A](n: Expr[A])(using Quotes): Expr[Long] =
    if (isIntType[A](n))
      '{Int.MinValue.toLong} // toLong avoids boxing
    else
      '{Long.MinValue}

  private def checkedImpl[A](n: Expr[A], fallback: Expr[Nothing])(using Quotes, Type[A]): Expr[A] =
    import quotes.reflect.*

    val tree: Term = n.asTerm
    val numLimit = limit[A](n)
    val isInt = isIntType(n)
    val isLong = isLongType(n)
    // report.info(s"${n.show} $isInt $isLong")

    val acc = new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        report.info(s"term ${n.show} ${tree.tpe.show}")
        tree match
          case Select(x, "unary_-") =>
            val isInt = isIntType(n)
            val isLong = isLongType(n)
            if (isInt)
              '{
                  val z = ${toInt(checkedImpl(x.asExprOf[Any], fallback))}
                  if (z == ${numLimit}) $fallback else { (-z).asInstanceOf[A] }
              }.asTerm
            else if (isLong)
              '{
                  val z = ${toLong(checkedImpl(x.asExprOf[Any], fallback))}
                  if (z == ${numLimit}) $fallback else { (-z).asInstanceOf[A] }
              }.asTerm
            else super.transformTerm(tree)(owner)
          // NOTE I couldn't find a way to unify the long and int branches. Suggestions are welcome
          case Apply(Select(x, "*"), List(y)) =>
            val isInt = isIntType(n)
            val isLong = isLongType(n)
            if (isInt)
              '{
                val xt = ${toInt(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toInt(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt * yt
                if (xt == 0  || (yt == z / xt && !(xt == -1 && yt == $numLimit))) z else $fallback
              }.asTerm
            else if (isLong)
              '{
                val xt = ${toLong(checkedImpl(x.asExprOf[Any], fallback))}
                val yt = ${toLong(checkedImpl(y.asExprOf[Any], fallback))}
                val z = xt * yt
                if (xt == 0  || (yt == z / xt && !(xt == -1 && yt == $numLimit))) z else $fallback
              }.asTerm
            else super.transformTerm(tree)(owner)
          case Apply(Select(x, "+"), List(y)) =>
            val isInt = isIntType(n)
            val isLong = isLongType(n)
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
            val isInt = isIntType(n)
            val isLong = isLongType(n)
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
            val isInt = isIntType(n)
            val isLong = isLongType(n)
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

    acc.transformTerm(tree)(tree.symbol).asExprOf[A]

  /**
   * Performs overflow checking for Int/Long operations.
 *
   * If no errors are detected, the expected result will be returned
   * in a Some wrapper. If an error is detected, None will be
   * returned.
   */
  inline def option(inline n: Long): Option[Long] =
    // NOTE: We may be able to inline this to make the macro fallback to None
    try
      Some(checked(n))
    catch
      case a: ArithmeticOverflowException => None

  inline def option(inline n: Int): Option[Int] =
    try
      Some(checked(n))
    catch
      case a: ArithmeticOverflowException => None

  inline def option[A](inline n: A): Option[A] =
    Some(n)

  /**
   * Performs overflow checking for Int/Long operations.
   *
   * If no errors are detected, the expected result will be
   * returned. If there are errors, the 'orElse' block will be
   * evaluated and returned.
   *
   * In the error case, this macro will actually evaluate a return
   * statement in the outer method context. Thus, it should only be
   * called from within a method that you would like to "return out
   * of" in the case of an overflow.
   */
  inline def tryOrReturn[A](n: Int)(orElse: Int): Int = option(n).getOrElse(orElse)
  inline def tryOrReturn[A](n: Long)(orElse: Long): Long = option(n).getOrElse(orElse)
  inline def tryOrReturn[A](n: A)(orElse: A): A = option(n).getOrElse(orElse)



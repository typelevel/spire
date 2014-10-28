package spire.macros

import scala.language.existentials

import language.experimental.macros

import spire.macros.compat.{resetLocalAttrs, termName, Context}

case class ArithmeticOverflowException(message: String) extends ArithmeticException(message)


object Checked {
  /**
   * Rewrites the expression provided to check many Int/Long arithmetic
   * operations for overflow (specifically +, -, *, / and unary_-). If an
   * overflow is detected, an `ArithmeticOverflowException` is thrown.
   */
  def checked[A](n: A): A = macro checkedImpl[A]

  def checkedImpl[A](c: Context)(n: c.Expr[A]): c.Expr[A] = {
    val tree = CheckedRewriter[c.type](c)(n.tree)
    val resetTree = resetLocalAttrs(c)(tree) // See SI-6711
    c.Expr[A](resetTree)
  }

  /**
   * This is equivalent to wrapping `checked` in a `Some` in a try/catch block
   * and if an `ArithmeticOverflowException` is caught, then `None` is returned
   * instead.
   */
  def option[A](n: A): Option[A] = macro optionImpl[A]

  def optionImpl[A](c: Context)(n: c.Expr[A]): c.Expr[Option[A]] = {
    val checkedExpr = checkedImpl[A](c)(n)
    c.universe.reify {
      try {
        Some(checkedExpr.splice)
      } catch { case (ex: ArithmeticOverflowException) =>
        None
      }
    }
  }

  @inline
  private final def overflowLong: Nothing = throw new ArithmeticOverflowException("Long arithmetic overflow")

  @inline
  private final def overflowInt: Nothing = throw new ArithmeticOverflowException("Int arithmetic overflow")

  final def negate(x: Long): Long = if (x != Long.MinValue) -x else overflowLong

  final def plus(x: Long, y: Long): Long = {
    val z = x + y
    if ((~(x ^ y) & (x ^ z)) >= 0L) z else overflowLong
  }

  final def minus(x: Long, y: Long): Long = {
    val z = x - y
    if (((x ^ y) & (x ^ z)) >= 0L) z else overflowLong
  }

  final def times(x: Long, y: Long): Long = {
    val z = x * y
    if (x == 0L || (y == z / x && !(x == -1L && y == Long.MinValue))) z else overflowLong
  }

  final def div(x: Long, y: Long): Long = {
    val z = x / y
    if (y != -1L || x != Long.MinValue) z else overflowLong
  }

  final def negate(x: Int): Int = if (x != Int.MinValue) -x else overflowInt

  final def plus(x: Int, y: Int): Int = {
    val z = x + y
    if ((~(x ^ y) & (x ^ z)) >= 0L) z else overflowInt
  }

  final def minus(x: Int, y: Int): Int = {
    val z = x - y
    if (((x ^ y) & (x ^ z)) >= 0L) z else overflowInt
  }

  final def times(x: Int, y: Int): Int = {
    val z = x * y
    if (x == 0L || (y == z / x && !(x == -1L && y == Int.MinValue))) z else overflowInt
  }

  final def div(x: Int, y: Int): Int = {
    val z = x / y
    if (y != -1L || x != Int.MinValue) z else overflowInt
  }
}

private[macros] case class CheckedRewriter[C <: Context](c: C) {
  import c.universe._

  def apply(tree: Tree): Tree = {
    tree match {
      case Literal(_) => c.warning(tree.pos, "checked used with literal")
      case Ident(_) => c.warning(tree.pos, "checked used with simple identifier")
      case _ =>
    }

    rewriter.transform(tree)
  }

  private object rewriter extends Transformer {
    val rewrite = IntRewriter(transform _) orElse LongRewriter(transform _)

    override def transform(tree: Tree): Tree =
      if (rewrite isDefinedAt tree) rewrite(tree) else super.transform(tree)
  }

  private sealed abstract class Rewriter[A](implicit typeTag: c.WeakTypeTag[A]) {
    def tpe: Type = typeTag.tpe

    private val unaryOps = Map("unary_-" -> q"spire.macros.Checked.negate")

    private val binaryOps = Map(
      "+" -> q"spire.macros.Checked.plus",
      "-" -> q"spire.macros.Checked.minus",
      "*" -> q"spire.macros.Checked.times",
      "/" -> q"spire.macros.Checked.div"
    )

    def isCheckableUnop(tree: Tree): Boolean = tree match {
      case Select(lhs, method) if (unaryOps contains method.decodedName.toString) && (lhs.tpe.widen <:< tpe) => true
      case _ => false
    }

    // At least one has to conform strongly to tpe.
    def binopConforms(lt: Type, rt: Type): Boolean =
      ((lt weak_<:< tpe) && (rt <:< tpe)) || ((lt <:< tpe) && (rt weak_<:< tpe))
    
    def isCheckableBinop(tree: Tree): Boolean = tree match {
      case Apply(Select(lhs, method), rhs :: Nil) =>
        (binaryOps contains method.decodedName.toString) && binopConforms(lhs.tpe.widen, rhs.tpe.widen)
      case _ =>
        false
    }

    def apply(rewrite: Tree => Tree): PartialFunction[Tree, Tree] = {
      case tree @ Select(sub, method) if isCheckableUnop(tree) =>
        q"${unaryOps(method.decodedName.toString)}(${rewrite(sub)})"

      case tree @ Apply(Select(lhs, method), rhs :: Nil) if isCheckableBinop(tree) =>
        q"${binaryOps(method.decodedName.toString)}(${rewrite(lhs)}, ${rewrite(rhs)})"
    }
  }

  private object LongRewriter extends Rewriter[Long]

  private object IntRewriter extends Rewriter[Int]
}

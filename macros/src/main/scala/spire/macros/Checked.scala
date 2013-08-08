package spire.macros

import language.experimental.macros
import scala.reflect.macros.Context

object Checked {
  def apply[A](n: A): A = macro checkedImpl[A]

  def checked[A](n: A): A = macro checkedImpl[A]

  def checkedImpl[A](c: Context)(n: c.Expr[A]): c.Expr[A] = {
    val tree = CheckedRewriter[c.type](c)(n.tree)
    val resetTree = c.resetLocalAttrs(tree) // See SI-6711
    c.Expr[A](resetTree)
  }

  private final def overflowLong: Nothing = throw new ArithmeticException("Long arithmetic overflow")
  private final def overflowInt: Nothing = throw new ArithmeticException("Int arithmetic overflow")

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
      case Select(lhs, method) if (unaryOps contains method.decoded) && (lhs.tpe.widen <:< tpe) => true
      case _ => false
    }

    // At least one has to conform strongly to tpe.
    def binopConforms(lt: Type, rt: Type): Boolean =
      ((lt weak_<:< tpe) && (rt <:< tpe)) || ((lt <:< tpe) && (rt weak_<:< tpe))
    
    def isCheckableBinop(tree: Tree): Boolean = tree match {
      case Apply(Select(lhs, method), rhs :: Nil) if (binaryOps contains method.decoded) && binopConforms(lhs.tpe.widen, rhs.tpe.widen) => true
      case _ => false
    }

    def apply(rewrite: Tree => Tree): PartialFunction[Tree, Tree] = {
      case tree @ Select(sub, method) if isCheckableUnop(tree) =>
        q"${unaryOps(method.decoded)}(${rewrite(sub)})"

      case tree @ Apply(Select(lhs, method), rhs :: Nil) if isCheckableBinop(tree) =>
        q"${binaryOps(method.decoded)}(${rewrite(lhs)}, ${rewrite(rhs)})"
    }
  }

  private object LongRewriter extends Rewriter[Long]

  private object IntRewriter extends Rewriter[Int]
}

package spire.macros

import language.experimental.macros
import scala.reflect.macros.Context

object Checked {
  def apply[A](n: A): A = macro checkedImpl[A]

  def checkedImpl[A](c: Context)(n: c.Expr[A]): c.Expr[A] =
    c.Expr[A](CheckedRewriter[c.type](c)(n.tree))

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

  private object LongRewriter extends Rewriter[Long] {
    def negateCheck(x: c.Expr[Long], y: c.Expr[Long]) =
      reify { x.splice != Long.MinValue }

    def plusCheck(x: c.Expr[Long], y: c.Expr[Long], z: c.Expr[Long]) =
      reify { (~(x.splice ^ y.splice) & (x.splice ^ z.splice)) >= 0L }

    def minusCheck(x: c.Expr[Long], y: c.Expr[Long], z: c.Expr[Long]) =
      reify { ((x.splice ^ y.splice) & (x.splice ^ z.splice)) >= 0L }

    def timesCheck(x: c.Expr[Long], y: c.Expr[Long], z: c.Expr[Long]) =
      reify { x.splice == 0L || (y.splice == z.splice / x.splice && !(x.splice == -1L && y.splice == Long.MinValue)) }

    def divCheck(x: c.Expr[Long], y: c.Expr[Long], z: c.Expr[Long]) =
      reify { y.splice != -1L || x.splice != Long.MinValue }
  }

  private object IntRewriter extends Rewriter[Int] {
    def negateCheck(x: c.Expr[Int], y: c.Expr[Int]) =
      reify { x.splice != Int.MinValue }

    def plusCheck(x: c.Expr[Int], y: c.Expr[Int], z: c.Expr[Int]) =
      reify { (~(x.splice ^ y.splice) & (x.splice ^ z.splice)) >= 0 }

    def minusCheck(x: c.Expr[Int], y: c.Expr[Int], z: c.Expr[Int]) =
      reify { ((x.splice ^ y.splice) & (x.splice ^ z.splice)) >= 0 }

    def timesCheck(x: c.Expr[Int], y: c.Expr[Int], z: c.Expr[Int]) =
      reify { x.splice == 0 || (y.splice == z.splice / x.splice && !(x.splice == -1 && y.splice == Int.MinValue)) }

    def divCheck(x: c.Expr[Int], y: c.Expr[Int], z: c.Expr[Int]) =
      reify { y.splice != -1 || x.splice != Int.MinValue }
  }

  private abstract class Rewriter[A](implicit typeTag: c.WeakTypeTag[A]) {
    def negateCheck(x: c.Expr[A], y: c.Expr[A]): c.Expr[Boolean]

    def plusCheck(x: c.Expr[A], y: c.Expr[A], z: c.Expr[A]): c.Expr[Boolean]

    def minusCheck(x: c.Expr[A], y: c.Expr[A], z: c.Expr[A]): c.Expr[Boolean]

    def timesCheck(x: c.Expr[A], y: c.Expr[A], z: c.Expr[A]): c.Expr[Boolean]

    def divCheck(x: c.Expr[A], y: c.Expr[A], z: c.Expr[A]): c.Expr[Boolean]

    def tpe: Type = typeTag.tpe

    val overflow = {
      val message = c.Expr[String](Literal(Constant(tpe.toString + " arithmetic overflow")))
      reify { throw new ArithmeticException(message.splice) }
    }

    def assignVal[A](tree: Tree): (c.Expr[A], ValDef) = {
      val tmpName = c.fresh("checked$")
      val tmp = ValDef(NoMods, tmpName, TypeTree(), tree)
      (c.Expr[A](Ident(newTermName(tmpName))), tmp)
    }

    def unop[A](lhs: Tree, name: Name)(check: (c.Expr[A], c.Expr[A]) => c.Expr[Boolean]): Tree = {
      val (x, xValDef) = assignVal[A](lhs)
      val (y, yValDef) = assignVal[A](Select(x.tree, name))
      Block(List(xValDef, yValDef), If(check(x, y).tree, y.tree, overflow.tree))
    }

    def binop[A](lhs: Tree, rhs: Tree, name: Name)
        (check: (c.Expr[A], c.Expr[A], c.Expr[A]) => c.Expr[Boolean]): Tree = {
      val (x, xValDef) = assignVal[A](lhs)
      val (y, yValDef) = assignVal[A](rhs)
      val (z, zValDef) = assignVal[A](Apply(Select(x.tree, name), y.tree :: Nil))
      Block(List(xValDef, yValDef, zValDef), If(check(x, y, z).tree, z.tree, overflow.tree))
    }

    def isUnop(name: String)(tree: Tree): Boolean = tree match {
      case Select(lhs, method) if (method.decoded == name) && (lhs.tpe <:< tpe) => true
      case _ => false
    }

    // At least one has to conform strongly to tpe.
    def binopConforms(lt: Type, rt: Type): Boolean = {
      ((lt weak_<:< tpe) && (rt <:< tpe)) || ((lt <:< tpe) && (rt weak_<:< tpe))
    }
    
    def isBinop(name: String)(tree: Tree): Boolean = tree match {
      case Apply(Select(lhs, method), rhs :: Nil) if method.decoded == name && binopConforms(lhs.tpe, rhs.tpe) => true
      case _ => false
    }

    def apply(rewrite: Tree => Tree): PartialFunction[Tree, Tree] = {
      case tree @ Select(lhs, method) if isUnop("unary_-")(tree) =>
        unop[A](rewrite(lhs), method)(negateCheck)

      case tree @ Apply(Select(lhs, method), rhs :: Nil) if isBinop("+")(tree) =>
        binop[A](rewrite(lhs), rewrite(rhs), method)(plusCheck)

      case tree @ Apply(Select(lhs, method), rhs :: Nil) if isBinop("-")(tree) =>
        binop[A](rewrite(lhs), rewrite(rhs), method)(minusCheck)

      case tree @ Apply(Select(lhs, method), rhs :: Nil) if isBinop("*")(tree) =>
        binop[A](rewrite(lhs), rewrite(rhs), method)(timesCheck)

      case tree @ Apply(Select(lhs, method), rhs :: Nil) if isBinop("/")(tree) =>
        binop[A](rewrite(lhs), rewrite(rhs), method)(divCheck)
    }
  }
}

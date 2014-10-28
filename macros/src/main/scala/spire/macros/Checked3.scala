package spire.macros

import scala.language.existentials

import language.experimental.macros

import spire.macros.compat.{resetLocalAttrs, termName, Context}

object Checked3 {
  def tryOrReturn[A](n: A)(orElse: A): A = macro tryOrReturnImpl[A]

  def tryOrReturnImpl[A](c: Context)(n: c.Expr[A])(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val tree = CheckedRewriter3[c.type](c)(n.tree)
    val resetTree = resetLocalAttrs(c)(tree) // See SI-6711
    c.Expr[A](q"{ def fallback = $orElse; $resetTree }")
  }
}

private[macros] case class CheckedRewriter3[C <: Context](c: C) {
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

  private sealed abstract class Rewriter[A](minValue: Tree)(implicit typeTag: c.WeakTypeTag[A]) {
    def tpe: Type = typeTag.tpe

    // unops
    val Negate = termName(c)("unary_$minus")

    // binops
    val Plus = termName(c)("$plus")
    val Minus = termName(c)("$minus")
    val Times = termName(c)("$times")
    val Div = termName(c)("$div")
    val Mod = termName(c)("$percent")

    def unopOk(tree: Tree): Boolean = tree match {
      case Select(lhs, method) => lhs.tpe.widen <:< tpe
      case _ => false
    }

    def binopOk(tree: Tree): Boolean = tree match {
      case Apply(Select(lhs, method), rhs :: Nil) =>
        val lt = lhs.tpe.widen
        val rt = rhs.tpe.widen
        ((lt weak_<:< tpe) && (rt <:< tpe)) || ((lt <:< tpe) && (rt weak_<:< tpe))
      case _ =>
        false
    }

    def apply(rewrite: Tree => Tree): PartialFunction[Tree, Tree] = {
      case tree @ Select(sub, Negate) if unopOk(tree) =>
        q"""{ val x = $sub; if (x == $minValue) return fallback; -x }"""

      case tree @ Apply(Select(lhs, Plus), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x + y; if ((~(x ^ y) & (x ^ z)) < 0) return fallback; z }"""

      case tree @ Apply(Select(lhs, Minus), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x - y; if (((x ^ y) & (x ^ z)) < 0L) return fallback; z }"""

      case tree @ Apply(Select(lhs, Times), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x * y; if (x == 0 || (y == z / x && !(x == -1 && y == $minValue))) z else return fallback }"""

      case tree @ Apply(Select(lhs, Div), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x / y; if (y == -1 && x == $minValue) return fallback; z }"""

      case tree @ Apply(Select(lhs, Mod), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x % y; if (y == -1 && x == $minValue) return fallback; z }"""
    }
  }

  private object LongRewriter extends Rewriter[Long](q"Long.MinValue")

  private object IntRewriter extends Rewriter[Int](q"Int.MinValue")
}

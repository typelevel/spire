package spire.macros

import scala.language.existentials

import language.experimental.macros

import spire.macros.compat.{resetLocalAttrs, termName, Context}

object Checked2 {
  def tryOrElse[A](n: A)(orElse: A): A = macro tryOrElseImpl[A]

  def tryOrElseImpl[A](c: Context)(n: c.Expr[A])(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val tree = CheckedRewriter2[c.type](c)(n.tree)
    val resetTree = resetLocalAttrs(c)(tree) // See SI-6711
    c.Expr[A](q"{ var ok = true; val z = $resetTree; if (ok) z else $orElse }")
  }
}

private[macros] case class CheckedRewriter2[C <: Context](c: C) {
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
        q"""{ val x = $sub; if (x == $minValue) { ok = false }; -x }"""

      case tree @ Apply(Select(lhs, Plus), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x + y; if ((~(x ^ y) & (x ^ z)) < 0) { ok = false }; z }"""

      case tree @ Apply(Select(lhs, Minus), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x - y; if (((x ^ y) & (x ^ z)) < 0L) { ok = false }; z }"""

      case tree @ Apply(Select(lhs, Times), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x * y; if (x == 0 || (y == z / x && !(x == -1 && y == $minValue))) () else { ok = false }; z }"""

      case tree @ Apply(Select(lhs, Div), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x / y; if (y == -1 && x == $minValue) { ok = false }; z }"""

      case tree @ Apply(Select(lhs, Mod), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = $lhs; val y = $rhs; val z = x % y; if (y == -1 && x == $minValue) { ok = false }; z }"""
    }
  }

  private object LongRewriter extends Rewriter[Long](q"Long.MinValue")

  private object IntRewriter extends Rewriter[Int](q"Int.MinValue")
}

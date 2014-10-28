package spire.macros

import scala.language.existentials

import language.experimental.macros

import spire.macros.compat.{resetLocalAttrs, termName, freshTermName, Context}

object Checked4 {
  def tryOrReturn[A](n: A)(orElse: A): A = macro tryOrReturnImpl[A]

  def tryOrReturnImpl[A: c.WeakTypeTag](c: Context)(n: c.Expr[A])(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val tree = CheckedRewriter4[c.type](c).rewrite[A](n.tree, orElse.tree)
    val resetTree = resetLocalAttrs(c)(tree) // See SI-6711
    c.Expr[A](resetTree)
  }
}

private[macros] case class CheckedRewriter4[C <: Context](c: C) {
  import c.universe._

  def rewrite[A: c.WeakTypeTag](tree: Tree, fallback: Tree): Tree = {
    tree match {
      case Literal(_) => c.warning(tree.pos, "checked used with literal")
      case Ident(_) => c.warning(tree.pos, "checked used with simple identifier")
      case _ =>
    }

    val A = weakTypeOf[A]
    val fallbackMethod = freshTermName(c)("checked$fallback$")
    val attemptMethod = freshTermName(c)("checked$attempt$")
    val LongRewriter = new Rewriter[Long](q"Long.MinValue", fallbackMethod)
    val IntRewriter = new Rewriter[Int](q"Int.MinValue", fallbackMethod)
    object rewriter extends Transformer {
      val rewrite = IntRewriter(transform _) orElse LongRewriter(transform _)
      override def transform(tree: Tree): Tree =
        if (rewrite isDefinedAt tree) rewrite(tree) else super.transform(tree)
    }
    val attempt = rewriter.transform(tree)
    
    q"""{
    def $fallbackMethod: $A = $fallback
    def $attemptMethod: $A = $attempt
    $attemptMethod
    }"""
  }

  private class Rewriter[A](minValue: Tree, fallback: TermName)(implicit typeTag: c.WeakTypeTag[A]) {
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
        q"""{ val x = ${rewrite(sub)}; if (x == $minValue) return $fallback; -x }"""

      case tree @ Apply(Select(lhs, Plus), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = ${rewrite(lhs)}; val y = ${rewrite(rhs)}; val z = x + y; if ((~(x ^ y) & (x ^ z)) < 0) return $fallback; z }"""

      case tree @ Apply(Select(lhs, Minus), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = ${rewrite(lhs)}; val y = ${rewrite(rhs)}; val z = x - y; if (((x ^ y) & (x ^ z)) < 0L) return $fallback; z }"""

      case tree @ Apply(Select(lhs, Times), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = ${rewrite(lhs)}; val y = ${rewrite(rhs)}; val z = x * y; if (x == 0 || (y == z / x && !(x == -1 && y == $minValue))) z else return $fallback }"""

      case tree @ Apply(Select(lhs, Div), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = ${rewrite(lhs)}; val y = ${rewrite(rhs)}; val z = x / y; if (y == -1 && x == $minValue) return $fallback; z }"""

      case tree @ Apply(Select(lhs, Mod), rhs :: Nil) if binopOk(tree) =>
        q"""{ val x = ${rewrite(lhs)}; val y = ${rewrite(rhs)}; val z = x % y; if (y == -1 && x == $minValue) return $fallback; z }"""
    }
  }
}


package spire
package macros

import scala.language.existentials
import spire.macros.compat.{freshTermName, resetLocalAttrs, termName, Context}

class ArithmeticOverflowException() extends ArithmeticException("arithmetic overflow detected")

object Checked {

  /**
   * Performs overflow checking for Int/Long operations.
   *
   * If no errors are detected, the expected result will be
   * returned. If there are errors, the 'orElse' block will be
   * evaluated and returned.
   */
  def tryOrElse[A](n: A)(orElse: A): A = macro tryOrElseImpl[A]

  /**
   * Performs overflow checking for Int/Long operations.
   *
   * If no errors are detected, the expected result will be
   * returned. If an error is detected, an ArithmeticOverflowException
   * will be thrown.
   */
  def checked[A](n: A): A = macro checkedImpl[A]

  /**
   * Performs overflow checking for Int/Long operations.
   *
   * If no errors are detected, the expected result will be returned
   * in a Some wrapper. If an error is detected, None will be
   * returned.
   */
  def option[A](n: A): Option[A] = macro optionImpl[A]

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
  def tryOrReturn[A](n: A)(orElse: A): A = macro tryOrReturnImpl[A]

  def tryOrElseImpl[A: c.WeakTypeTag](c: Context)(n: c.Expr[A])(orElse: c.Expr[A]): c.Expr[A] = {
    val tree = CheckedRewriter[c.type](c).rewriteSafe[A](n.tree, orElse.tree)
    val resetTree = resetLocalAttrs(c)(tree) // See SI-6711
    c.Expr[A](resetTree)
  }

  def checkedImpl[A: c.WeakTypeTag](c: Context)(n: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    tryOrElseImpl[A](c)(n)(c.Expr[A](q"throw new spire.macros.ArithmeticOverflowException()"))
  }

  def optionImpl[A: c.WeakTypeTag](c: Context)(n: c.Expr[A]): c.Expr[Option[A]] = {
    import c.universe._
    tryOrElseImpl[Option[A]](c)(c.Expr[Option[A]](q"Option(${n.tree})"))(c.Expr[Option[A]](q"None"))
  }

  def tryOrReturnImpl[A: c.WeakTypeTag](c: Context)(n: c.Expr[A])(orElse: c.Expr[A]): c.Expr[A] = {
    val tree = CheckedRewriter[c.type](c).rewriteFast[A](n.tree, orElse.tree)
    val resetTree = resetLocalAttrs(c)(tree) // See SI-6711
    c.Expr[A](resetTree)
  }
}

private[macros] case class CheckedRewriter[C <: Context](c: C) {
  import c.universe._

  def rewriteSafe[A: c.WeakTypeTag](tree: Tree, fallback: Tree): Tree = {
    warnOnSimpleTree(tree)
    val A = weakTypeOf[A]
    val aname = freshTermName(c)("checked$attempt$")
    val fname = freshTermName(c)("checked$fallback$")
    val attempt: Tree = makeRewriter(fname).transform(tree)
    q"""{ def $fname: $A = $fallback; def $aname: $A = $attempt; $aname }"""
  }

  def rewriteFast[A: c.WeakTypeTag](tree: Tree, fallback: Tree): Tree = {
    warnOnSimpleTree(tree)
    val A = weakTypeOf[A]
    val fname = freshTermName(c)("checked$fallback$")
    val attempt: Tree = makeRewriter(fname).transform(tree)
    q"""{ def $fname: $A = $fallback; $attempt }"""
  }

  def warnOnSimpleTree(tree: Tree): Unit =
    tree match {
      case Literal(_) => c.warning(tree.pos, "checked used with literal")
      case Ident(_)   => c.warning(tree.pos, "checked used with simple identifier")
      case _          =>
    }

  def makeRewriter(fallback: TermName): Transformer = {
    val LongRewriter = new Rewriter[Long](q"Long.MinValue", fallback)
    val IntRewriter = new Rewriter[Int](q"Int.MinValue", fallback)

    new Transformer {
      val f = IntRewriter(transform _).orElse(LongRewriter(transform _))

      override def transform(tree: Tree): Tree =
        if (f.isDefinedAt(tree)) f(tree) else super.transform(tree)
    }
  }

  class Rewriter[A](val minValue: Tree, val fallback: TermName)(implicit typeTag: c.WeakTypeTag[A]) {
    val tpe: Type = typeTag.tpe

    // unops
    val Negate = termName(c)("unary_$minus")

    // binops
    val Plus = termName(c)("$plus")
    val Minus = termName(c)("$minus")
    val Times = termName(c)("$times")
    val Div = termName(c)("$div")
    val Mod = termName(c)("$percent")

    def binopOk(tree: Tree): Boolean = tree match {
      case Apply(Select(lhs, method), rhs :: Nil) =>
        val lt = lhs.tpe.widen
        val rt = rhs.tpe.widen
        ((lt.weak_<:<(tpe)) && (rt <:< tpe)) || ((lt <:< tpe) && (rt.weak_<:<(tpe)))
      case _ =>
        false
    }

    def isSimple(tree: Tree): Boolean =
      tree match {
        case Literal(_) | Ident(_) => true
        case _                     => false
      }

    def runWithX(rewrite: Tree => Tree, sub: Tree)(f: Tree => Tree): Tree =
      if (isSimple(sub)) q"""{ ${f(sub)} }"""
      else {
        val x = freshTermName(c)("x$")
        q"""{ val $x = ${rewrite(sub)}; ${f(q"$x")} }"""
      }

    def runWithXYZ(rewrite: Tree => Tree, lhs: Tree, rhs: Tree)(f: (Tree, Tree, Tree) => Tree): Tree = {
      val z = freshTermName(c)("z$")
      if (isSimple(lhs) && isSimple(rhs)) {
        val t = f(lhs, rhs, q"$z")
        q"""{ $t }"""
      } else if (isSimple(lhs)) {
        val y = freshTermName(c)("y$")
        val t = f(lhs, q"$y", q"$z")
        q"""{ val $y = ${rewrite(rhs)}; $t }"""
      } else if (isSimple(rhs)) {
        val x = freshTermName(c)("x$")
        val t = f(q"$x", rhs, q"$z")
        q"""{ val $x = ${rewrite(lhs)}; $t }"""
      } else {
        val x = freshTermName(c)("x$")
        val y = freshTermName(c)("y$")
        val t = f(q"$x", q"$y", q"$z")
        q"""{ val $x = ${rewrite(lhs)}; val $y = ${rewrite(rhs)}; $t }"""
      }
    }

    def apply(rewrite: Tree => Tree): PartialFunction[Tree, Tree] = {
      case tree @ Select(sub, Negate) if sub.tpe.widen <:< tpe =>
        runWithX(rewrite, sub) { x =>
          q"""if ($x == $minValue) return $fallback; -$x"""
        }

      case tree @ Apply(Select(lhs, Plus), rhs :: Nil) if binopOk(tree) =>
        runWithXYZ(rewrite, lhs, rhs) { (x, y, z) =>
          q"""val $z = $x + $y; if ((~($x ^ $y) & ($x ^ $z)) < 0) return $fallback; $z"""
        }

      case tree @ Apply(Select(lhs, Minus), rhs :: Nil) if binopOk(tree) =>
        runWithXYZ(rewrite, lhs, rhs) { (x, y, z) =>
          q"""val $z = $x - $y; if ((($x ^ $y) & ($x ^ $z)) < 0L) return $fallback; $z"""
        }

      case tree @ Apply(Select(lhs, Times), rhs :: Nil) if binopOk(tree) =>
        runWithXYZ(rewrite, lhs, rhs) { (x, y, z) =>
          q"""val $z = $x * $y; if ($x == 0 || ($y == $z / $x && !($x == -1 && $y == $minValue))) $z else return $fallback"""
        }

      case tree @ Apply(Select(lhs, Div), rhs :: Nil) if binopOk(tree) =>
        runWithXYZ(rewrite, lhs, rhs) { (x, y, z) =>
          q"""val $z = $x / $y; if ($y == -1 && $x == $minValue) return $fallback; $z"""
        }

      case tree @ Apply(Select(lhs, Mod), rhs :: Nil) if binopOk(tree) =>
        runWithXYZ(rewrite, lhs, rhs) { (x, y, z) =>
          q"""val $z = $x % $y; if ($y == -1 && $x == $minValue) return $fallback; $z"""
        }
    }
  }
}

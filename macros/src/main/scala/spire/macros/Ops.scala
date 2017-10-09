package spire
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}

object Ops extends machinist.Ops {

  def uesc(c: Char): String = "$u%04X".format(c.toInt)

  val operatorNames: Map[String, String] =
    machinist.DefaultOps.operatorNames ++ Map(
      // partial operations |+|? |+|?? |-|? |-|??
      ("$bar$plus$bar$qmark$qmark", "opIsDefined"),
      ("$bar$minus$bar$qmark$qmark", "opInverseIsDefined"),
      ("$bar$plus$bar$qmark", "partialOp"),
      ("$bar$minus$bar$qmark", "partialOpInverse"),

      // partial actions ?|+|> ??|+|> <|+|? <|+|??
      ("$qmark$bar$plus$bar$greater", "partialActl"),
      ("$qmark$qmark$bar$plus$bar$greater", "actlIsDefined"),
      ("$less$bar$plus$bar$qmark", "partialActr"),
      ("$less$bar$plus$bar$qmark$qmark", "actrIsDefined"),

      // square root
      (uesc('√'), "sqrt"),

      // equality, comparisons
      (uesc('≡'), "eqv"),
      (uesc('≠'), "neqv"),
      (uesc('≤'), "lteqv"),
      (uesc('≥'), "gteqv"),

      // lattices/heyting
      (uesc('∧'), "meet"),
      (uesc('∨'), "join"),
      (uesc('⊃'), "imp"),
      (uesc('¬'), "complement"),

      // bool
      (uesc('⊻'), "xor"),
      (uesc('⊼'), "nand"),
      (uesc('⊽'), "nor")
    )

  def eqv[A, B](c: Context)(rhs: c.Expr[B])(ev: c.Expr[A =:= B]): c.Expr[Boolean] = {
    import c.universe._
    val (e, lhs) = unpack(c)
    c.Expr[Boolean](q"$e.eqv($lhs, $rhs)")
  }

  def neqv[A, B](c: Context)(rhs: c.Expr[B])(ev: c.Expr[A =:= B]): c.Expr[Boolean] = {
    import c.universe._
    val (e, lhs) = unpack(c)
    c.Expr[Boolean](q"$e.neqv($lhs, $rhs)")
  }

  /**
   * Like [[binop]] and [[binopWithEv]], but there is ev provided by the implicit
   * constructor, and ev1 provided by the method.
   *
   * If we see code like:
   *
   * {{{
   *   lhs.gcd(rhs)
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs)(ev: Ev).gcd(rhs)(ev1: Ev1): R
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.gcd(lhs, rhs)(ev1): R
   * }}}
   *
   * @group macros
   */
  def binopWithEv2[A, Ev1, R](c: Context)(rhs: c.Expr[A])(ev1: c.Expr[Ev1]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Apply(Select(ev, findMethodName(c)), List(lhs, rhs.tree)), List(ev1.tree)))
  }

}

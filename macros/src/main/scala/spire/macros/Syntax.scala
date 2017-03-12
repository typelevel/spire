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

      // truncated division /~ % /%
      ("t_$div$tilde", "tquot"),
      ("t_$percent", "tmod"),
      ("t_$div$percent", "tquotmod"),
      ("f_$div$tilde", "fquot"),
      ("f_$percent", "fmod"),
      ("f_$div$percent", "fquotmod"),

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

case class SyntaxUtil[C <: Context with Singleton](val c: C) {

  import c.universe._

  def name(s: String) = freshTermName(c)(s + "$")

  def names(bs: String*) = bs.toList.map(name)

  def isClean(es: c.Expr[_]*): Boolean =
    es.forall {
      _.tree match {
        case t @ Ident(_: TermName) if t.symbol.asTerm.isStable => true
        case Function(_, _) => true
        case _ => false
      }
    }
}

class InlineUtil[C <: Context with Singleton](val c: C) {
  import c.universe._

  def inlineAndReset[T](tree: Tree): c.Expr[T] = {
    val inlined = inlineApplyRecursive(tree)
    c.Expr[T](resetLocalAttrs(c)(inlined))
  }

  def inlineApplyRecursive(tree: Tree): Tree = {
    val ApplyName = termName(c)("apply")

    class InlineSymbol(name: TermName, symbol: Symbol, value: Tree) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree: Ident if tree.symbol == symbol =>
          if (tree.name == name) {
            value
          }
          else {
            super.transform(tree)
          }

        case tt: TypeTree if tt.original != null =>
          //super.transform(TypeTree().setOriginal(transform(tt.original)))
          super.transform(setOrig(c)(TypeTree(), transform(tt.original)))
        case _ =>
          super.transform(tree)
      }
    }

    object InlineApply extends Transformer {
      def inlineSymbol(name: TermName, symbol: Symbol, body: Tree, arg: Tree): Tree =
        new InlineSymbol(name, symbol, arg).transform(body)

      override def transform(tree: Tree): Tree = tree match {
        case Apply(Select(Function(params, body), ApplyName), args) =>
          params.zip(args).foldLeft(body) { case (b, (param, arg)) =>
            inlineSymbol(param.name, param.symbol, b, arg)
          }

        case Apply(Function(params, body), args) =>
          params.zip(args).foldLeft(body) { case (b, (param, arg)) =>
            inlineSymbol(param.name, param.symbol, b, arg)
          }

        case _ =>
          super.transform(tree)
      }
    }

    InlineApply.transform(tree)
  }
}

object Syntax {

  def cforMacro[A](c: Context)(init: c.Expr[A])
     (test: c.Expr[A => Boolean], next: c.Expr[A => A])
     (body: c.Expr[A => Unit]): c.Expr[Unit] = {


    import c.universe._
    val util = SyntaxUtil[c.type](c)
    val index = util.name("index")

    /**
     * If our arguments are all "clean" (anonymous functions or simple
     * identifiers) then we can go ahead and just inline them directly
     * into a while loop.
     *
     * If one or more of our arguments are "dirty" (something more
     * complex than an anonymous function or simple identifier) then
     * we will go ahead and bind each argument to a val just to be
     * safe.
     */
    val tree = if (util.isClean(test, next, body)) {
      q"""
      var $index = $init
      while ($test($index)) {
        $body($index)
        $index = $next($index)
      }
      """

    } else {
      val testName = util.name("test")
      val nextName = util.name("next")
      val bodyName = util.name("body")

      q"""
      val $testName: Int => Boolean = $test
      val $nextName: Int => Int = $next
      val $bodyName: Int => Unit = $body
      var $index: Int = $init
      while ($testName($index)) {
        $bodyName($index)
        $index = $nextName($index)
      }
      """
    }

    /**
     * Instead of just returning 'tree', we will go ahead and inline
     * anonymous functions which are immediately applied.
v     */
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def cforRangeMacro(c: Context)(r: c.Expr[Range])(body: c.Expr[Int => Unit]): c.Expr[Unit] = {

    import c.universe._
    val util = SyntaxUtil[c.type](c)

    val List(range, index, end, limit, step) =
      util.names("range", "index", "end", "limit", "step")

    def isLiteral(t: Tree): Option[Int] = t match {
      case Literal(Constant(a)) => a match {
        case n: Int => Some(n)
        case _ => None
      }
      case _ => None
    }

    def strideUpTo(fromExpr: Tree, toExpr: Tree, stride: Int): Tree =
      q"""
      var $index: Int = $fromExpr
      val $end: Int = $toExpr
      while ($index <= $end) {
        $body($index)
        $index += $stride
      }"""

    def strideUpUntil(fromExpr: Tree, untilExpr: Tree, stride: Int): Tree =
      q"""
      var $index: Int = $fromExpr
      val $limit: Int = $untilExpr
      while ($index < $limit) {
        $body($index)
        $index += $stride
      }"""

    def strideDownTo(fromExpr: Tree, toExpr: Tree, stride: Int): Tree =
      q"""
      var $index: Int = $fromExpr
      val $end: Int = $toExpr
      while ($index >= $end) {
        $body($index)
        $index -= $stride
      }"""

    def strideDownUntil(fromExpr: Tree, untilExpr: Tree, stride: Int): Tree =
      q"""
      var $index: Int = $fromExpr
      val $limit: Int = $untilExpr
      while ($index > $limit) {
        $body($index)
        $index -= $stride
      }"""

    val predef = spire.macros.compat.predef(c)

    val tree: Tree = r.tree match {

      case q"$predef.intWrapper($i).until($j)" =>
        strideUpUntil(i, j, 1)

      case q"$predef.intWrapper($i).to($j)" =>
        strideUpTo(i, j, 1)

      case r @ q"$predef.intWrapper($i).until($j).by($step)" =>
        isLiteral(step) match {
          case Some(k) if k > 0 => strideUpUntil(i, j, k)
          case Some(k) if k < 0 => strideDownUntil(i, j, -k)
          case Some(k) if k == 0 =>
            c.error(c.enclosingPosition, "zero stride")
            q"()"
          case None =>
            c.info(c.enclosingPosition, "non-literal stride", true)
            q"$r.foreach($body)"
        }

      case r @ q"$predef.intWrapper($i).to($j).by($step)" =>
        isLiteral(step) match {
          case Some(k) if k > 0 => strideUpTo(i, j, k)
          case Some(k) if k < 0 => strideDownTo(i, j, -k)
          case Some(k) if k == 0 =>
            c.error(c.enclosingPosition, "zero stride")
            q"()"
          case None =>
            c.info(c.enclosingPosition, "non-literal stride", true)
            q"$r.foreach($body)"
        }

      case r =>
        c.info(c.enclosingPosition, "non-literal range", true)
        q"$r.foreach($body)"
    }

    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def cforRange2Macro(c: Context)(r1: c.Expr[Range], r2: c.Expr[Range])
    (body: c.Expr[(Int, Int) => Unit]): c.Expr[Unit] = {

    import c.universe._
    c.Expr[Unit](q"cforRange($r1)(i => cforRange($r2)(j => $body(i, j)))")
  }
}

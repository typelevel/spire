package spire.macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context}

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

// This is Scala reflection source compatibility hack between Scala 2.10 and 2.11
private object HasCompat { val compat = ??? }; import HasCompat._

class InlineUtil[C <: Context with Singleton](val c: C) {
  import c.universe._
  // This is Scala reflection source compatibility hack between Scala 2.10 and 2.11
  import compat._

  def inlineAndReset[T](tree: Tree): c.Expr[T] = {
    val inlined = inlineApplyRecursive(tree)
    c.Expr[T](resetLocalAttrs(c)(inlined))
  }

  def inlineApplyRecursive(tree: Tree): Tree = {
    val ApplyName = termName(c)("apply")

    class InlineSymbol(symbol: Symbol, value: Tree) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(_) if tree.symbol == symbol =>
          value
        case tt: TypeTree if tt.original != null =>
          super.transform(TypeTree().setOriginal(transform(tt.original)))
        case _ =>
          super.transform(tree)
      }
    }

    object InlineApply extends Transformer {
      def inlineSymbol(symbol: Symbol, body: Tree, arg: Tree): Tree =
        new InlineSymbol(symbol, arg).transform(body)

      override def transform(tree: Tree): Tree = tree match {
        case Apply(Select(Function(params, body), ApplyName), args) =>
          params.zip(args).foldLeft(body) { case (b, (param, arg)) =>
            inlineSymbol(param.symbol, b, arg)
          }
          
        case Apply(Function(params, body), args) =>
          params.zip(args).foldLeft(body) { case (b, (param, arg)) =>
            inlineSymbol(param.symbol, b, arg)
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

    val tree: Tree = r.tree match {

      case q"scala.this.Predef.intWrapper($i).until($j)" =>
        strideUpUntil(i, j, 1)

      case q"scala.this.Predef.intWrapper($i).to($j)" =>
        strideUpTo(i, j, 1)

      case r @ q"scala.this.Predef.intWrapper($i).until($j).by($step)" =>
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

      case r @ q"scala.this.Predef.intWrapper($i).to($j).by($step)" =>
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

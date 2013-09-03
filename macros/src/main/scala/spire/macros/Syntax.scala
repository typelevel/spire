package spire.macros

import scala.reflect.macros.Context

case class SyntaxUtil[C <: Context with Singleton](val c: C) {
  import c.universe._
  import definitions._
  import Flag._

  def name(s: String) = newTermName(c.fresh(s + "$"))

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
    c.Expr[T](c.resetAllAttrs(inlined))
  }

  def inlineApplyRecursive(tree: Tree): Tree = {
    val ApplyName = newTermName("apply")

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
    val index = util.name("index")

    val IntWrapperName = newTermName("intWrapper")
    val ToName = newTermName("to")
    val UntilName = newTermName("until")
    val ByName = newTermName("by")

    val tree = r.tree match {
      case Apply(Select(Apply(Select(_, IntWrapperName), List(start)), UntilName), List(limit)) =>
        val limitName = util.name("limit")
q"""
var $index: Int = $start
val $limitName: Int = $limit
while ($index < $limitName) {
  $body($index)
  $index += 1
}
"""

      case Apply(Select(Apply(Select(Apply(Select(_, IntWrapperName), List(start)), UntilName), List(limit)), ByName), List(step)) =>
        val limitName = util.name("limit")
        val stepName = util.name("step")
q"""
var $index: Int = $start
val $limitName: Int = $limit
val $stepName: Int = $step
while ($index < $limitName) {
  $body($index)
  $index += $stepName
}
"""

      case Apply(Select(Apply(Select(_, IntWrapperName), List(start)), ToName), List(end)) =>
        val endName = util.name("end")
        q"""
var $index: Int = $start
val $endName: Int = $end
while ($index <= $endName) {
  $body($index)
  $index += 1
}
"""

      case Apply(Select(Apply(Select(Apply(Select(_, IntWrapperName), List(start)), ToName), List(end)), ByName), List(step)) =>
        val endName = util.name("end")
        val stepName = util.name("step")
        q"""
var $index: Int = $start
val $endName: Int = $end
val $stepName: Int = $step
while ($index <= $endName) {
  $body($index)
  $index += $stepName
}
"""

      case r =>
        c.info(c.enclosingPosition, "non-literal range found", true)
        println(showRaw(r))
        val range = util.name("range")
        val limit = util.name("limit")
        val step = util.name("step")

        q"""
val $range = $r
var $index: Int = $range.start
val $step: Int = $range.step
val $limit: Int = if ($range.isInclusive) $range.end + 1 else $range.end
while ($index < $limit) {
  $body($index)
  $index += $step
}
"""
    }

    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def cforRange2Macro(c: Context)(r1: c.Expr[Range], r2: c.Expr[Range])
    (body: c.Expr[(Int, Int) => Unit]): c.Expr[Unit] = {

    import c.universe._
    c.Expr[Unit](q"cforRange($r1)(i => cforRange($r2)(j => $body(i, j)))")
  }
}

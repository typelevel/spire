package spire.macrosk

import scala.reflect.macros.Context

object Syntax {

  def cforMacro[A: c.WeakTypeTag](c: Context)(init: c.Expr[A])
     (test: c.Expr[A => Boolean], next: c.Expr[A => A])
     (body: c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._
    import definitions._
    import Flag._

    val c.WeakTypeTag(tpe) = implicitly[c.WeakTypeTag[A]]

    def isClean(t: Tree): Boolean = t match {
      case Ident(_: TermName) if t.symbol.asTerm.isStable => true
      case Function(_, _) => true
      case _ => false
    }

    def name(s: String) = newTermName(c.fresh(s + "$cfor"))

    val clean = isClean(test.tree) && isClean(next.tree) && isClean(body.tree)

    val index = name("index")
    val whileLoop = name("while")

    val tree = if (clean) {
      /**
       * If our arguments are all "clean" (anonymous functions or
       * simple identifiers) then we can go ahead and just inline
       * them directly into a while loop.
       */
      q"""
      var $index = $init
      while ($test($index)) {
        $body($index)
        $index = $next($index)
      }
      """

    } else {

      /**
       * If one or more of our arguments are "dirty" (something more
       * complex than an anonymous function or simple identifier) then
       * we will go ahead and bind each argument to a val just to be
       * safe.
       */
      val testName = name("test")
      val nextName = name("next")
      val bodyName = name("body")

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
     */
    new Util[c.type](c).inlineAndReset[Unit](tree)
  }

  class Util[C <: Context with Singleton](val c:C) {
    import c.universe._

    def inlineAndReset[T](tree: Tree): c.Expr[T] =
      c.Expr[T](c.resetAllAttrs(inlineApplyRecursive(tree)))

    def inlineApplyRecursive(tree: Tree): Tree = {
      val ApplyName = newTermName("apply")

      class InlineSymbol(symbol: Symbol, value: Tree) extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Ident(_) if tree.symbol == symbol => value
          case _ => super.transform(tree)
        }
      }

      object InlineApply extends Transformer {
        def inlineSymbol(symbol: Symbol, body: Tree, arg: Tree): Tree =
          new InlineSymbol(symbol, arg).transform(body)

        override def transform(tree: Tree): Tree = tree match {
          case Apply(Select(Function(List(param), body), ApplyName), List(arg)) =>
            inlineSymbol(param.symbol, body, arg)
  
          case Apply(Function(List(param), body), List(arg)) =>
            inlineSymbol(param.symbol, body, arg)
  
          case _ =>
            super.transform(tree)
        }
      }

      InlineApply.transform(tree)
    }
  }
}

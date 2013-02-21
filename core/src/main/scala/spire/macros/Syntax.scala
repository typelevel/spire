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
      case Ident(_) => true
      case Function(_, _) => true
      case _ => false
    }

    def name(s: String) = newTermName(c.fresh(s + "$cfor"))

    val clean = isClean(test.tree) && isClean(next.tree) && isClean(body.tree)

    val tree = if (clean) {
      /**
       * If our arguments are all "clean" (anonymous functions or
       * simple identifiers) then we can go ahead and just inline
       * them directly into a while loop.
       */
      val index = name("index")
      val whileLoop = name("while")
      Block(
        ValDef(Modifiers(MUTABLE), index, TypeTree(tpe), init.tree), 
        LabelDef(
          whileLoop,
          List(),
          If(
            Apply(test.tree, List(Ident(index))),
            Block(
              Apply(body.tree, List(Ident(index))),
              Assign(Ident(index), Apply(next.tree, List(Ident(index)))),
              Apply(Ident(whileLoop), List())
            ),
            Literal(Constant(())))))

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

      val f1 = Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Function1"))
      val unit = Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Unit"))
      val bool = Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Boolean"))

      val index = name("index")
      val whileLoop = name("while")
      Block(
        ValDef(Modifiers(), testName, AppliedTypeTree(f1, List(TypeTree(tpe), bool)), test.tree),
        ValDef(Modifiers(), nextName, AppliedTypeTree(f1, List(TypeTree(tpe), TypeTree(tpe))), next.tree),
        ValDef(Modifiers(), bodyName, AppliedTypeTree(f1, List(TypeTree(tpe), unit)), body.tree),
        ValDef(Modifiers(MUTABLE), index, TypeTree(tpe), init.tree), 
        LabelDef(
          whileLoop,
          List(),
          If(
            Apply(Ident(testName), List(Ident(index))),
            Block(
              Apply(Ident(bodyName), List(Ident(index))),
              Assign(Ident(index), Apply(Ident(nextName), List(Ident(index)))),
              Apply(Ident(whileLoop), List())
            ),
            Literal(Constant(())))))
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

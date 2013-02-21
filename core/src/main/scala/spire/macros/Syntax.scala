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

    val index = newTermName(c.fresh)
    val whileLoop = newTermName(c.fresh)

    val tree = Block(
      ValDef(Modifiers(MUTABLE), index, TypeTree(tpe), init.tree), 
      LabelDef(
        whileLoop,
        List(),
        If(
          Apply(test.tree, List(Ident(index))),
          Block(
            Apply(body.tree, List(Ident(index))),
            Assign(Ident(i), Apply(next.tree, List(Ident(index)))),
            Apply(Ident(whileLoop), List())
          ),
          Literal(Constant(())))))

    new Util[c.type](c).inlineAndReset[Unit](tree)
  }

  class Util[C <: Context with Singleton](val c:C) {
    import c.universe._

    def die(msg:String) = c.abort(c.enclosingPosition, msg)

    def inlineAndReset[T](tree: Tree): c.Expr[T] =
      c.Expr[T](c resetAllAttrs inlineApplyRecursive(tree))

    def inlineApplyRecursive(tree: Tree): Tree = {
      val ApplyName = newTermName("apply")

      class InlineSymbol(symbol: Symbol, value:Tree) extends Transformer {
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

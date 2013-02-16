package spire.macrosk

import scala.reflect.macros.Context

object Syntax {
  def cforMacro[A:c.WeakTypeTag](c:Context)(init:c.Expr[A])
     (test:c.Expr[A => Boolean], next:c.Expr[A => A])
     (body:c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._

    val c.WeakTypeTag(tpe) = implicitly[c.WeakTypeTag[A]]

    val a = newTermName(c.fresh)
    val w = newTermName(c.fresh)

    val tailrec = newTermName("tailrec")

    val importTailrec = Import(
      Select(Ident("scala"), newTermName("annotation")),
      List(ImportSelector(tailrec, 1161, tailrec, 1161)))

    val mods = Modifiers(NoFlags, tpnme.EMPTY, List())

    val param = List(ValDef(Modifiers(Flag.PARAM), a, TypeTree(tpe), EmptyTree))

    val t = Block(
      List(
        DefDef(mods, w, List(), List(param), Ident(newTypeName("Unit")),
          If(Apply(test.tree, List(Ident(a))),
            Block(
              List(Apply(body.tree, List(Ident(a)))),
              Apply(Ident(w), List(Apply(next.tree, List(Ident(a)))))),
            Literal(Constant(()))))),
      Apply(Ident(w), List(init.tree)))

    new Util[c.type](c).inlineAndReset[Unit](t)
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

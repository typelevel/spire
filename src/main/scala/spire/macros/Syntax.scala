package spire.macrosk

import scala.reflect.macros.Context

object Syntax {
  // used to give our labels, vars a somewhat unique identifier
  var n: Int = 0

  def cforMacro[A:c.WeakTypeTag](c:Context)(init:c.Expr[A])
     (test:c.Expr[A => Boolean], next:c.Expr[A => A])
     (body:c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._

    val c.WeakTypeTag(tpe) = implicitly[c.WeakTypeTag[A]]

    // it would be good to do something smarter here
    n = (n + 1) % 100000

    val a = newTermName("a%d" format n)
    val w = newTermName("w%d" format n)

    val tailrec = newTermName("tailrec")

    val importTailrec = Import(
      Select(Ident("scala"), newTermName("annotation")),
      List(ImportSelector(tailrec, 1161, tailrec, 1161)))

    // TODO: why can't i get the freaking tailrec working??? ARGHGGH!!

    //val mods = Modifiers(NoFlags, tpnme.EMPTY,
    //  List(Apply(Select(New(Ident(tailrec)), nme.CONSTRUCTOR), List())))
    val mods = Modifiers(NoFlags, tpnme.EMPTY, List())

    val param = List(ValDef(Modifiers(Flag.PARAM), a, TypeTree(tpe), EmptyTree))

    val t = Block(
      List(
        //importTailrec,
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

      class InlineTerm(name:TermName, value:Tree) extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Ident(`name`) => value
          case _ => super.transform(tree)
        }
      }

      object InlineApply extends Transformer {
        def inlineTerms(params: List[Tree], body: Tree, args: List[Tree]): Tree = {
          if (params.length != args.length)
            die("bad arity: %s vs %s" format (params.length, args.length))

          params.zip(args).foldLeft(body) {
            case (body, (ValDef(_, name, _, _), arg)) => {
              new InlineTerm(name, arg).transform(body)
            }
          }
        }

        override def transform(tree: Tree): Tree = tree match {
          case Apply(Select(Function(params, body), ApplyName), args) =>
            inlineTerms(params, body, args)

          case Apply(Function(params, body), args) =>
            inlineTerms(params, body, args)

          case _ =>
            super.transform(tree)
        }
      }

      InlineApply.transform(tree)
    }
  }
}

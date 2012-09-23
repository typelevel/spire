package spire.macros

import language.implicitConversions
import language.higherKinds
import language.experimental.macros

import scala.reflect.macros.Context

object Syntax {

  def cfor[A](init:A)
    (test:A => Boolean, next:A => A)
    (body:A => Unit): Unit = macro cforMacro[A]

  def cforMacro[A:c.AbsTypeTag](c:Context)(init:c.Expr[A])
     (test:c.Expr[A => Boolean], next:c.Expr[A => A])
     (body:c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._

    //val iName = c.fresh("i$")
    //val cforName = c.fresh("cfor$")
    // TODO: use a fresh name instead of "elem_priv"
    // but to do that, we probably have to build the tree by hand
    val t = c.universe.reify {
      var elem_priv:A = init.splice
      while(test.splice(elem_priv)) {
        body.splice(elem_priv)
        elem_priv = next.splice(elem_priv)
      }
    }

    new Util[c.type](c).inlineAndReset(t)
  }

  class Util[C <: Context with Singleton](val c:C) {
    import c.universe._

    def die(msg:String) = c.abort(c.enclosingPosition, msg)

    def inlineAndReset[T](expr: c.Expr[T]): c.Expr[T] =
      c.Expr[T](c resetAllAttrs inlineApplyRecursive(expr.tree))

    def inlineApplyRecursive(tree: Tree): Tree = {
      val ApplyName = newTermName("apply")

      class InlineTerm(name:TermName, value:Tree) extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Ident(`name`) => value
          case _ => super.transform(tree)
        }
      }

      object InlineApply extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Apply(Select(Function(params, body), ApplyName), args) =>
            if (params.length != args.length)
              die("bad arity: %s vs %s" format (params.length, args.length))

            params.zip(args).foldLeft(body) {
              case (body, (ValDef(_, name, _, _), arg)) =>
                new InlineTerm(name, arg).transform(body)
            }

          case _ => super.transform(tree)
        }
      }

      InlineApply.transform(tree)
    }
  }
}

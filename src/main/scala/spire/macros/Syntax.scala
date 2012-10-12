package spire.macros

import language.implicitConversions
import language.higherKinds
import language.experimental.macros

import scala.reflect.macros.Context

object Syntax {

  def cfor[A](init:A)
    (test:A => Boolean, next:A => A)
    (body:A => Unit): Unit = macro cforMacro[A]

  def cforMacro[A:c.WeakTypeTag](c:Context)(init:c.Expr[A])
     (test:c.Expr[A => Boolean], next:c.Expr[A => A])
     (body:c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._
    val t = reify {
      import scala.annotation.tailrec
      @tailrec def loop(a: A) {
        if (test.splice(a)) {
          body.splice(a)
          loop(next.splice(a))
        }
      }
      loop(init.splice)
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

package spire.macros

import language.implicitConversions
import language.higherKinds
import language.experimental.macros
import scala.{specialized => spec}
import scala.reflect.makro.Context

import spire.math._
import spire.algebra._

/**
 * This trait has some nice methods for working with implicit Ops classes.
 */
object Ops {
  /**
   * Given context, this method pulls 'evidence' and 'lhs' values out of
   * instantiations of implicit -Ops classes. For instance,
   *
   * Given "new FooOps(x)(ev)", this method returns (ev, x).
   */
  def unpack[T[_], A](c:Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(x)), List(ev)) => (ev, x)
      case t => sys.error("bad tree: %s" format t)
    }
  }

  /**
   * Given context and a method name, this method rewrites the tree to call the
   * given method with the lhs parameter. This is useful when defining unary
   * operators as macros, for instance: !x, -x.
   */
  def unop[R](c:Context)(name:String):c.Expr[R] = {
    import c.universe._
    val (ev, x) = unpack(c)
    c.Expr[R](Apply(Select(ev, name), List(x)))
  }

  /**
   * Given context, an expression, and a method name, this method rewrites the
   * tree to call the given method with the lhs and rhs parameters. This is
   * useful when defining binary operators as macros, for instance: x * y.
   */
  def binop[A, R](c:Context)(y:c.Expr[A], name:String):c.Expr[R] = {
    import c.universe._
    val (ev, x) = unpack(c)
    c.Expr[R](Apply(Select(ev, name), List(x, y.tree)))
  }
}

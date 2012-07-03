package spire.macros

import language.implicitConversions
import language.higherKinds
import language.experimental.macros
import scala.{specialized => spec}
import scala.reflect.makro.Context

import spire.math._
import spire.algebra._

object Macros {
  // eq
  def eqv[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, Boolean](c)(rhs, "eqv")
  def neqv[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, Boolean](c)(rhs, "neqv")

  // order
  def gt[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, Boolean](c)(rhs, "gt")
  def gteqv[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, Boolean](c)(rhs, "gteqv")
  def lt[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, Boolean](c)(rhs, "lt")
  def lteqv[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, Boolean](c)(rhs, "lteqv")
  def compare[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, Int](c)(rhs, "compare")
  def min[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "min")
  def max[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "max")

  // ring
  def negate[A](c:Context)() = Ops.unop[A](c)("negate")
  def plus[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "plus")
  def times[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "times")
  def minus[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "minus")
  def pow[A](c:Context)(rhs:c.Expr[Int]) = Ops.binop[Int, A](c)(rhs, "pow")

  // euclidean ring
  def quot[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "quot")
  def mod[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "mod")
  def quotmod[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, (A, A)](c)(rhs, "quotmod")

  // field
  def div[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "div")
  def isWhole[A](c:Context)() = Ops.unop[Boolean](c)("isWhole")

  // fractional
  def ceil[A](c:Context)() = Ops.unop[A](c)("ceil")
  def floor[A](c:Context)() = Ops.unop[A](c)("floor")

  // nroot
  def nroot[A](c:Context)(rhs:c.Expr[Int]) = Ops.binop[Int, A](c)(rhs, "nroot")
  def sqrt[A](c:Context)() = Ops.unop[A](c)("sqrt")

  // semigroup
  def op[A](c:Context)(rhs:c.Expr[A]) = Ops.binop[A, A](c)(rhs, "op")

  // signed
  def abs[A](c:Context)() = Ops.unop[A](c)("abs")
  def sign[A](c:Context)() = Ops.unop[Sign](c)("sign")
  def signum[A](c:Context)() = Ops.unop[Int](c)("signum")

  // convertable
  def toByte[A](c:Context)() = Ops.unop[Byte](c)("toByte")
  def toShort[A](c:Context)() = Ops.unop[Short](c)("toShort")
  def toInt[A](c:Context)() = Ops.unop[Int](c)("toInt")
  def toLong[A](c:Context)() = Ops.unop[Long](c)("toLong")
  def toFloat[A](c:Context)() = Ops.unop[Float](c)("toFloat")
  def toDouble[A](c:Context)() = Ops.unop[Double](c)("toDouble")
  def toBigInt[A](c:Context)() = Ops.unop[BigInt](c)("toBigInt")
  def toBigDecimal[A](c:Context)() = Ops.unop[BigDecimal](c)("toBigDecimal")
  def toRational[A](c:Context)() = Ops.unop[Rational](c)("toRational")  
}

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

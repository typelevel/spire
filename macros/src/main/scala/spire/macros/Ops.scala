package spire.macrosk

import scala.reflect.macros.Context

/**
 * This trait has some nice methods for working with implicit Ops classes.
 */
trait Ops {

  /**
   * Given context, this method rewrites the tree to call the desired method
   * with the lhs parameter. We find the symbol which is applying the macro
   * and use its name to determine what method to call.
   *
   * Users write code like:
   *
   *   -x
   *
   * After typing and implicit resolution, we get trees like:
   *   
   *   ringOps[A](x:A)(ev:R[A]).unary_-()
   *
   * and we want to get out:
   *
   *   ev.negate(x:A)
   *
   * So, we need to decompose ringOps[A](x)(ev) to get x and ev, and we need
   * to map "unary_-" into "negate".
   */
  def unop[R](c:Context)():c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)

    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs)))
  }

  def unopWithEv[Ev, R](c:Context)(ev: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val lhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(ev.tree, findMethodName(c)), List(lhs)))
  }


  def flip[A, R](c: Context)(rhs: c.Expr[A]): c.Expr[R] = {
    import c.universe._
    val lhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(rhs.tree, findMethodName(c)), List(lhs)))
  }

  /**
   * Given context and an expression, this method rewrites the tree to call the
   * "desired" method with the lhs and rhs parameters. We find the symbol which
   * is applying the macro and use its name to determine what method to call.
   *
   * Users write code like:
   *
   *   x + y
   *
   * After typing and implicit resolution, we get trees like:
   *   
   *   ringOps[A](x:A)(ev:R[A]).+(y:A)
   *
   * and we want to get out:
   *
   *   ev.method(x:A, y:A)
   *
   * So, we need to decompose ringOps[A](x)(ev) to get x and ev, and we need
   * to map "+" into "plus".
   */
  def binop[A, R](c:Context)(rhs:c.Expr[A]):c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs, rhs.tree)))
  }

  /**
   * Like binop, but for right-associative operators (eg. +:).
   */
  def rbinop[A, R](c:Context)(lhs:c.Expr[A]):c.Expr[R] = {
    import c.universe._
    val (ev, rhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs.tree, rhs)))
  }

  def binopWithEv[A, Ev, R](c: Context)(rhs: c.Expr[A])(ev:c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val lhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(ev.tree, findMethodName(c)), List(lhs, rhs.tree)))
  }

  def rbinopWithEv[A, Ev, R](c: Context)(lhs: c.Expr[A])(ev:c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val rhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(ev.tree, findMethodName(c)), List(lhs.tree, rhs)))
  }

  /**
   * Given an expression like: xyz(lhs)(ev0).plus(rhs: Abc)(ev1)
   * This will create a tree like: ev0.plus(lhs, ev1.fromAbc(rhs))
   * Notably, this let's us use Ring's fromInt method and ConvertableTo's
   * fromDouble (etc.) before applying an op.
   */
  def binopWithLift[A: c.WeakTypeTag, Ev, R](c: Context)(rhs: c.Expr[A])(ev1: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val (ev0, lhs) = unpack(c)
    val typeName = weakTypeOf[A].typeSymbol.name
    val rhs1 = Apply(Select(ev1.tree, newTermName("from" + typeName)), List(rhs.tree))
    c.Expr[R](Apply(Select(ev0, findMethodName(c)), List(lhs, rhs1)))
  }

  def binopWithSelfLift[A: c.WeakTypeTag, Ev, R](c: Context)(rhs: c.Expr[A]): c.Expr[R] = {
    import c.universe._
    val (ev0, lhs) = unpack(c)
    val typeName = weakTypeOf[A].typeSymbol.name
    val rhs1 = Apply(Select(ev0, newTermName("from" + typeName)), List(rhs.tree))
    c.Expr[R](Apply(Select(ev0, findMethodName(c)), List(lhs, rhs1)))
  }

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
      case t => c.abort(c.enclosingPosition,
        "Cannot extract subject of operator (tree = %s)" format t)
    }
  }

  def unpackWithoutEv(c:Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) => lhs
      case t => c.abort(c.enclosingPosition,
        "Cannot extract subject of operator (tree = %s)" format t)
    }
  }

  /**
   * Provide a canonical mapping between "operator names" used in Ops classes
   * and the actual method names used for type classes.
   *
   * This is an interesting directory of the operators Spire supports. It's
   * also worth noting that we don't (currently) have the capacity to dispatch
   * to two different typeclass-method names for the same operator--typeclasses
   * have to agree to use the same name for the same operator.
   *
   * In general "textual" method names should just pass through to the
   * typeclass... it is probably not wise to provide mappings for them here.
   */
  def findMethodName(c:Context) = {
    import c.universe._
    val s = c.macroApplication.symbol.name.toString
    newTermName(operatorNames.getOrElse(s, s))
  }

  def operatorNames: Map[String, String]
}

object Ops extends Ops {
  final val operatorNames = Map(
    // Eq (=== =!=)
    ("$eq$eq$eq", "eqv"),
    ("$eq$bang$eq", "neqv"),

    // Order (> >= < <=)
    ("$greater", "gt"),
    ("$greater$eq", "gteqv"),
    ("$less", "lt"),
    ("$less$eq", "lteqv"),

    // Semigroup (|+|)
    ("$bar$plus$bar", "op"),
    ("$bar$minus$bar", "opInverse"),

    // Ring (unary_- + - * **)
    ("unary_$minus", "negate"),
    ("$plus", "plus"),
    ("$minus", "minus"),
    ("$times", "times"),
    ("$times$times", "pow"),

    // EuclideanRing (/~ % /%)
    ("$div$tilde", "quot"),
    ("$percent", "mod"),
    ("$div$percent", "quotmod"),

    // Field (/)
    ("$div", "div"),

    // BooleanAlgebra (^ | & ~)
    ("$up", "xor"),
    ("$bar", "or"),
    ("$amp", "and"),
    ("unary_$tilde", "complement"),

    // BitString (<< >> >>>)
    ("$less$less", "leftShift"),
    ("$greater$greater$greater", "rightShift"),
    ("$greater$greater", "signedRightShift"),

    // VectorSpace
    ("$times$colon", "timesl"),
    ("$colon$times", "timesr"),
    ("$colon$div", "divr"),
    ("$u22C5", "dot"),

    // GroupAction, Torsor, etc.
    ("$plus$greater", "gplusl"),
    ("$less$plus", "gplusr"),
    ("$times$greater", "gtimesl"),
    ("$less$times", "gtimesr"),
    ("$less$minus$greater", "pminus"),
    ("$less$div$greater", "pdiv")
  )
}

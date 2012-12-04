package spire.macrosk

import scala.reflect.macros.Context

import spire.math._
import spire.algebra._

/**
 * This trait has some nice methods for working with implicit Ops classes.
 */
object Ops {

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
    val s = c.macroApplication.symbol.name.toString
    val name = s match {
      // Eq (===, =!=)
      case "$eq$eq$eq" => "eqv"
      case "$eq$bang$eq" => "neqv"

      // Order (>, >=, <, <=)
      case "$greater" => "gt"
      case "$greater$eq" => "gteqv"
      case "$less" => "lt"
      case "$less$eq" => "lteqv"

      // Semigroup (|+|)
      case "$bar$plus$bar" => "op"

      // Ring (unary_-,+,-,*,**)
      case "unary_$minus" => "negate"
      case "$plus" => "plus"
      case "$minus" => "minus"
      case "$times" => "times"
      case "$times$times" => "pow"

      // EuclideanRing (/~,%,/%)
      case "$div$tilde" => "quot"
      case "$percent" => "mod"
      case "$div$percent" => "quotmod"

      // Field (/)
      case "$div" => "div"

      // BooleanAlgebra
      case "$up" => "xor"
      case "$bar" => "or"
      case "$amp" => "and"
      case "unary_$tilde" => "complement"

      // VectorSpace
      case "$times$colon" => "timesl"
      case "$colon$times" => "timesr"
      case "$u22C5" => "dot"

      case s => s
    }
    //println("s=%s -> name=%s" format (s, name))
    name
  }
}

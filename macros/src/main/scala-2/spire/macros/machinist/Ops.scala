package spire.macros.machinist

import scala.reflect.macros.blackbox.Context

/**
 * This trait has some nice methods for working with implicit `Ops`
 * classes. It is used to rewrite implicit conversions which "enrich"
 * a type with operators into code that does not allocate an implicit
 * instance.
 *
 * @groupname macros Macros
 * @groupdesc macros Macro transformations for operators
 */
trait Ops {

  /**
   * Given context, this method rewrites the tree to call the desired
   * method with the lhs parameter. We find the symbol which is
   * applying the macro and use its name to determine what method to
   * call.
   *
   * If we see code like:
   *
   * {{{
   *   -lhs
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs)(ev).unary_-(): R
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.negate(lhs): R
   * }}}
   *
   * @group macros
   */
  def unop[R](c: Context): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs)))
  }

  /**
   * A variant of [[unop]] which doesn't take an empty parameter list.
   *
   * @group macros
   */
  def unop0[R](c: Context): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs)))
  }

  /**
   * Like [[unop]], but with ev provided to the method instead of to the
   * implicit constructor.
   *
   * If we see code like:
   *
   * {{{
   *   lhs.abs
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs).abs(ev: Ev): R
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.abs(lhs): R
   * }}}
   *
   * @group macros
   */
  def unopWithEv[Ev, R](c: Context)(ev: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val lhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(ev.tree, findMethodName(c)), List(lhs)))
  }

  /**
   * Like [[unop]] and [[unopWithEv]], but there is ev provided by the implicit
   * constructor, and ev1 provided by the method.
   *
   * If we see code like:
   *
   * {{{
   *   lhs.isId
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs)(ev: Ev).isId(ev1: Ev1): R
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.isId(lhs)(ev1): R
   * }}}
   *
   * @group macros
   */
  def unopWithEv2[Ev1, R](c: Context)(ev1: c.Expr[Ev1]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Apply(Select(ev, findMethodName(c)), List(lhs)), List(ev1.tree)))
  }

  /**
   * Given context and an expression, this method rewrites the tree to
   * call the "desired" method with the lhs and rhs parameters. We
   * find the symbol which is applying the macro and use its name to
   * determine what method to call.
   *
   * If we see code like:
   *
   * {{{
   *   lhs + rhs
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs)(ev).\$plus(rhs: A): R
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.method(lhs: A, rhs: A): R
   * }}}
   *
   * @group macros
   */
  def binop[A, R](c: Context)(rhs: c.Expr[A]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs, rhs.tree)))
  }

  /**
   * Like [[binop]], but for right-associative operators (eg. `+:`).
   *
   * If we see code like:
   *
   * {{{
   *   lhs *: rhs
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(rhs)(ev).\$times\$colon(lhs)
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.timesl(lhs, rhs)
   * }}}
   *
   * @group macros
   */
  def rbinop[A, R](c: Context)(lhs: c.Expr[A]): c.Expr[R] = {
    import c.universe._
    val (ev, rhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs.tree, rhs)))
  }

  def unopWithScalar[R](c: Context)(): c.Expr[R] =
    handleUnopWithChild[R](c)("scalar")

  def unopWithScalar0[R](c: Context): c.Expr[R] =
    handleUnopWithChild[R](c)("scalar")

  def handleUnopWithChild[R](c: Context)(childName: String): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    val child = Select(ev, TermName(childName))
    c.Expr[R](Apply(Select(child, findMethodName(c)), List(lhs)))
  }

  /**
   * Like [[binop]], but where the implementation comes from a child member
   *
   * If we see code like:
   *
   * {{{
   *   lhs * rhs
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs)(ev).\$times(rhs)
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.scalar.times(lhs, rhs)
   * }}}
   *
   * @group macros
   */
  def binopWithScalar[A, R](c: Context)(rhs: c.Expr[A]): c.Expr[R] =
    handleBinopWithChild(c)(rhs)("scalar")

  /**
   * Provided to make defining things like [[binopWithScalar]] easier.
   *
   * @group macros
   */
  def handleBinopWithChild[A, R](c: Context)(rhs: c.Expr[A])(childName: String): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    val child = Select(ev, TermName(childName))
    c.Expr[R](Apply(Select(child, findMethodName(c)), List(lhs, rhs.tree)))
  }

  /**
   * Like [[binop]], but with ev provided to the method instead of to the
   * implicit constructor.
   *
   * If we see code like:
   *
   * {{{
   *   lhs % rhs
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs).\$percent(rhs)(ev)
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.mod(lhs, rhs)
   * }}}
   *
   * @group macros
   */
  def binopWithEv[A, Ev, R](c: Context)(rhs: c.Expr[A])(ev: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val lhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(ev.tree, findMethodName(c)), List(lhs, rhs.tree)))
  }

  /**
   * Like [[rbinop]], but with ev provided to the method instead of to the
   * implicit constructor.
   *
   * If we see code like:
   *
   * {{{
   *   lhs *: rhs
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(rhs).\$times\$colon(lhs)(ev)
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.timesl(lhs, rhs)
   * }}}
   *
   * @group macros
   */
  def rbinopWithEv[A, Ev, R](c: Context)(lhs: c.Expr[A])(ev: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val rhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(ev.tree, findMethodName(c)), List(lhs.tree, rhs)))
  }

  /**
   * Combine an implicit enrichment with a lifting method.
   *
   * If we see code like:
   *
   * {{{
   *   lhs + 1
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs)(ev0).\$plus(1)(ev1): R
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev0.plus(lhs, ev1.fromInt(1))
   * }}}
   *
   * In Spire, this lets us use `Ring`'s fromInt method and
   * `ConvertableTo`'s `fromDouble` (etc.) before applying an
   * op. Eventually, we should generalize the way we choose the
   * lifting method.
   *
   * @group macros
   */
  def binopWithLift[A: c.WeakTypeTag, Ev, R](c: Context)(rhs: c.Expr[A])(ev1: c.Expr[Ev]): c.Expr[R] = {
    import c.universe._
    val (ev0, lhs) = unpack(c)
    val typeName = weakTypeOf[A].typeSymbol.name
    val rhs1 = Apply(Select(ev1.tree, TermName("from" + typeName)), List(rhs.tree))
    c.Expr[R](Apply(Select(ev0, findMethodName(c)), List(lhs, rhs1)))
  }

  /**
   * This is like [[binopWithLift]], but we use the same evidence
   * parameter to make the method call and do the lifting.
   *
   * If we see code like:
   *
   * {{{
   *   lhs * 2
   * }}}
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs)(ev).\$times(2): R
   * }}}
   *
   * The macro should produce trees like:
   *
   * {{{
   *   ev.times(lhs, ev.fromInt(2))
   * }}}
   *
   * @group macros
   */
  def binopWithSelfLift[A: c.WeakTypeTag, Ev, R](c: Context)(rhs: c.Expr[A]): c.Expr[R] = {
    import c.universe._
    val (ev0, lhs) = unpack(c)
    val typeName = weakTypeOf[A].typeSymbol.name
    val rhs1 = Apply(Select(ev0, TermName("from" + typeName)), List(rhs.tree))
    c.Expr[R](Apply(Select(ev0, findMethodName(c)), List(lhs, rhs1)))
  }

  /**
   * Similar to [[binop]], but for situations where there is no evidence
   * parameter, and we just want to call a method on the rhs.
   *
   * After typing and implicit resolution, we get trees like:
   *
   * {{{
   *   conversion(lhs).foo(rhs)
   * }}}
   *
   * and we want to get out:
   *
   * {{{
   *   rhs.foo(lhs)
   * }}}
   *
   * @group macros
   */
  def flip[A, R](c: Context)(rhs: c.Expr[A]): c.Expr[R] = {
    import c.universe._
    val lhs = unpackWithoutEv(c)
    c.Expr[R](Apply(Select(rhs.tree, findMethodName(c)), List(lhs)))
  }

  /**
   * Given context, this method pulls the 'ev' and 'lhs' values out of
   * instantiations of implicit -`Ops` classes.
   *
   * For instance, given a tree like:
   *
   * {{{
   *   new FooOps(x)(ev)
   * }}}
   *
   * This method would return `(ev, x)`.
   *
   * @group macros
   */
  def unpack[T[_], A](c: Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(x)), List(ev)) => (ev, x)
      case t                                                => c.abort(c.enclosingPosition, "Cannot extract subject of operator (tree = %s)".format(t))
    }
  }

  /**
   * Given context, this method pulls the 'lhs' value out of
   * instantiations of implicit -`Ops` classes.
   *
   * For instance, given a tree like:
   *
   * {{{
   *   new FooOps(x)
   * }}}
   *
   * This method would return `x`.
   *
   * @group macros
   */
  def unpackWithoutEv(c: Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) => lhs
      case t                                 => c.abort(c.enclosingPosition, "Cannot extract subject of operator (tree = %s)".format(t))
    }
  }

  /**
   * Provide a canonical mapping between "operator names" used in `Ops`
   * classes and the actual method names used for type classes.
   *
   * It's worth noting that a particular instance of `Ops` must always
   * map a given symbol a single method name. If you want to be able
   * to map the same symbol to different names in different contexts,
   * you'll need to create multiple `Ops` instances and configure them
   * appropriately.
   *
   * In general "textual" method names should just pass through to the
   * typeclass--it is probably not wise to provide mappings for them
   * here.
   */
  def findMethodName(c: Context) = {
    import c.universe._
    val s = c.macroApplication.symbol.name.toString
    TermName(operatorNames.getOrElse(s, s))
  }

  /**
   * Map of symbolic -> textual name conversions.
   *
   * If this map is empty, the macros will not do any special
   * rewriting and all names will be passed through.
   *
   * Symbolic names should be written as Scala would represent them
   * internally. For example, `+` should be written as `\$plus`.
   */
  def operatorNames: Map[String, String]
}

trait DefaultOperatorNames {

  val operatorNames = Map(
    // Eq (=== =!=)
    ("$eq$eq$eq", "eqv"),
    ("$eq$bang$eq", "neqv"),
    // PartialOrder (> >= < <=)
    ("$greater", "gt"),
    ("$greater$eq", "gteqv"),
    ("$less", "lt"),
    ("$less$eq", "lteqv"),
    // Semigroup (|+| |-|)
    ("$bar$plus$bar", "combine"),
    ("$bar$minus$bar", "remove"),
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
    // VectorSpace (*: :* :/ ⋅)
    ("$times$colon", "timesl"),
    ("$colon$times", "timesr"),
    ("$colon$div", "divr"),
    ("$u22C5", "dot"),
    // GroupAction (|+|> <|+| +> <+ *> <*)
    ("$bar$plus$bar$greater", "actl"),
    ("$less$bar$plus$bar", "actr"),
    ("$plus$greater", "gplusl"),
    ("$less$plus", "gplusr"),
    ("$times$greater", "gtimesl"),
    ("$less$times", "gtimesr"),
    // Torsor (<|-|> <-> </>)
    ("$less$bar$minus$bar$greater", "pdiff"),
    ("$less$minus$greater", "pminus"),
    ("$less$div$greater", "pdiv")
  )
}

object DefaultOps extends Ops with DefaultOperatorNames

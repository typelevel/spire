package spire.macros

import scala.language.experimental.macros
import spire.macros.compat.Context

/**
 * Value class providing evidence that `value` is positive.
 *
 * You should not use the `new PositiveInt` constructor in your own
 * code -- it should be considered private, although the value class
 * technology requires it to remain public. Instead, you should
 * construct instances via the PositiveInt(_) factory constructor.
 */
class PositiveInt(val value: Int) extends AnyVal {
  override def toString: String = value.toString
}

object PositiveInt {

  /**
   * This is the preferred constructor for PositiveInt.
   *
   * If `n` is a literal, the sign test will occur at compile-time
   * instead of at runtime. Otherwise, a runtime check will be
   * performed.
   */
  def apply(n: Int): PositiveInt =
    macro verifyPositiveInt

  /**
   * Runtime constructor for PositiveInt.
   *
   * This method is used by apply when n's sign is not known at
   * compile time. There is no reason for user code to call this
   * method directly (although other than a sign check there is no
   * harm in it).
   */
  def check(n: Int): PositiveInt =
    if (n > 0) new PositiveInt(n)
    else throw new IllegalArgumentException(s"$n <= 0")

  /**
   * Macro which will do sign tests at compile-time.
   */
  def verifyPositiveInt(c: Context)(n: c.Expr[Int]): c.Expr[PositiveInt] = {
    import c.universe._
    c.Expr(n.tree match {
      case Literal(Constant(x: Int)) =>
        if (x > 0) {
          q"new _root_.spire.macros.PositiveInt($n)"
        } else {
          c.abort(c.enclosingPosition, s"$x <= 0")
        }
      case _ =>
        q"_root_.spire.macros.PositiveInt.check($n)"
    })
  }
}

package spire.macros

import scala.language.experimental.macros

import scala.reflect.macros.Context

case class PositiveInt(value: Int) {
  require(value > 0)
}

object PositiveInt {
  def buildPositiveInt(n: Int): PositiveInt = macro verifyPositiveInt

  def verifyPositiveInt(c: Context)(n: c.Expr[Int]): c.Expr[PositiveInt] = {
    import c.universe._

    val tree = n.tree match {
      case Literal(Constant(x: Int)) if x > 0 =>
        q"_root_.spire.macros.PositiveInt($n)"
      case Literal(Constant(x: Int)) =>
        c.abort(c.enclosingPosition, s"$x <= 0")
      case _ => 
        q"_root_.spire.macros.PositiveInt($n)"
    }
    c.Expr(tree)
  }
}

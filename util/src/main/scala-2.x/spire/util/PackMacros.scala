package spire
package util

import scala.language.experimental.macros
import spire.macros.compat.Context

trait PackMacros {
  @inline private[util] def ism(n: Int, shift: Int): Byte =
    ((n >>> shift) & 0xff).toByte

  @inline private[util] def lsm(n: Long, shift: Int): Byte =
    ((n >>> shift) & 0xffL).toByte

  /**
   * index must be 0 <= index < 4
   */
  def intToByte(n: Int)(index: Int): Byte = macro PackMacros.intToByteMacro

  /**
   * index must be 0 <= index < 8
   */
  def longToByte(n: Long)(index: Int): Byte = macro PackMacros.longToByteMacro
}

object PackMacros {

  def intToByteMacro(c: Context)(n: c.Expr[Int])(index: c.Expr[Int]): c.Expr[Byte] = {
    import c.universe._
    index.tree match {
      case Literal(Constant(i: Int)) =>
        if (0 <= i && i < 4) {
          val offset = c.Expr[Int](Literal(Constant(24 - i * 8)))
          reify { ((n.splice >>> offset.splice) & 0xff).toByte }
        } else {
          c.abort(c.enclosingPosition, "index outside of 0-3")
        }
      case _ =>
        reify { Pack.intToByteRuntime(n.splice)(index.splice) }
    }
  }

  def longToByteMacro(c: Context)(n: c.Expr[Long])(index: c.Expr[Int]): c.Expr[Byte] = {
    import c.universe._
    index.tree match {
      case Literal(Constant(i: Int)) =>
        if (0 <= i && i < 8) {
          val offset = c.Expr[Int](Literal(Constant(56 - i * 8)))
          reify { ((n.splice >>> offset.splice) & 0xff).toByte }
        } else {
          c.abort(c.enclosingPosition, "index outside of 0-7")
        }
      case _ =>
        reify { Pack.longToByteRuntime(n.splice)(index.splice) }
    }
  }
}

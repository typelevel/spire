package spire.algebra

import spire.math._
import spire.macrosk.Ops
import java.lang.Math

import scala.{specialized => spec}

trait Field[@spec(Int,Long,Float,Double) A] extends EuclideanRing[A] with MultiplicativeAbGroup[A] {
  def ceil(a:A): A
  def floor(a:A): A
  def round(a:A): A
  def isWhole(a:A): Boolean
}

final class FieldOps[A](lhs:A)(implicit ev:Field[A]) {
  def isWhole() = macro Ops.unop[Boolean]
  def ceil() = macro Ops.unop[A]
  def floor() = macro Ops.unop[A]
  def round() = macro Ops.unop[A]
}

object Field {
  @inline final def apply[A](implicit f:Field[A]):Field[A] = f
}

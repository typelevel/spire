package spire.algebra

import scala.{specialized => spec}

trait Trig[@spec(Float, Double) A] {
  def e: A
  def pi: A

  def exp(a: A): A
  def expm1(a: A): A
  def log(a:A): A
  def log1p(a: A): A

  def sin(a: A): A
  def cos(a: A): A
  def tan(a: A): A

  def asin(a: A): A
  def acos(a: A): A
  def atan(a: A): A
  def atan2(y: A, x: A): A

  def sinh(x: A): A
  def cosh(x: A): A
  def tanh(x: A): A

  def toRadians(a: A): A
  def toDegrees(a: A): A
}

object Trig {
  @inline final def apply[A](implicit t: Trig[A]) = t
}

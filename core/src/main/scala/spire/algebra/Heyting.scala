package spire.algebra

import scala.{specialized => sp}

trait Heyting[@sp(Boolean, Byte, Short, Int, Long) A] {
  def one: A
  def zero: A
  def complement(a: A): A
  def and(a: A, b: A): A
  def or(a: A, b: A): A
  def imp(a: A, b: A): A
}

object Heyting {
  @inline final def apply[@sp(Boolean, Byte, Short, Int, Long) A](implicit ev: Heyting[A]): Heyting[A] = ev
}

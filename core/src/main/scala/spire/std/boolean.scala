package spire.std

import spire.algebra._

trait BooleanIsBooleanAlgebra extends BooleanAlgebra[Boolean] {
  def one: Boolean = true
  def zero: Boolean = false
  def and(a: Boolean, b: Boolean): Boolean = a & b
  def or(a: Boolean, b: Boolean): Boolean = a | b
  def complement(a: Boolean): Boolean = !a
  override def xor(a: Boolean, b: Boolean): Boolean = a ^ b
}

trait BooleanIsRig extends Rig[Boolean] {
  def one: Boolean = true
  def plus(a:Boolean, b:Boolean): Boolean = a || b
  override def pow(a:Boolean, b:Int): Boolean = a
  override def times(a:Boolean, b:Boolean): Boolean = a && b
  def zero: Boolean = false
}

trait BooleanOrder extends Order[Boolean] {
  override def eqv(x:Boolean, y:Boolean): Boolean = x == y
  override def neqv(x:Boolean, y:Boolean): Boolean = x != y
  override def gt(x: Boolean, y: Boolean): Boolean = x && !y
  override def lt(x: Boolean, y: Boolean): Boolean = !x && y
  override def gteqv(x: Boolean, y: Boolean): Boolean = x == y || x
  override def lteqv(x: Boolean, y: Boolean): Boolean = x == y || y

  override def min(x: Boolean, y: Boolean): Boolean = x && y
  override def max(x: Boolean, y: Boolean): Boolean = x || y
  def compare(x: Boolean, y: Boolean): Int = if (x) {
    if (y) 0 else 1
  } else {
    if (y) -1 else 0
  }
}

trait BooleanInstances {
  implicit final val BooleanIsBooleanAlgebra = new BooleanIsBooleanAlgebra {}
  implicit final val BooleanIsRig = new BooleanIsRig {}
  implicit final val BoolOrder = new BooleanOrder {}
}

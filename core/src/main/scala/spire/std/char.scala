package spire.std

import spire.algebra._

trait CharEq extends Eq[Char] {
  def eqv(x:Char, y:Char) = x == y
  override def neqv(x:Char, y:Char) = x != y
}

trait CharOrder extends Order[Char] with CharEq {
  override def gt(x: Char, y: Char) = x > y
  override def gteqv(x: Char, y: Char) = x >= y
  override def lt(x: Char, y: Char) = x < y
  override def lteqv(x: Char, y: Char) = x <= y
  def compare(x: Char, y: Char) = if (x < y) -1 else if (x > y) 1 else 0
}

trait CharInstances {
  implicit object CharOrder extends CharOrder
}

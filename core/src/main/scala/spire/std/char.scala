package spire.std

import spire.algebra._

trait CharOrder extends Order[Char] {
  override def eqv(x:Char, y:Char) = x == y
  override def neqv(x:Char, y:Char) = x != y
  override def gt(x: Char, y: Char) = x > y
  override def gteqv(x: Char, y: Char) = x >= y
  override def lt(x: Char, y: Char) = x < y
  override def lteqv(x: Char, y: Char) = x <= y
  def compare(x: Char, y: Char) = if (x < y) -1 else if (x > y) 1 else 0
}

@SerialVersionUID(0L)
class CharAlgebra extends CharOrder with Serializable

trait CharInstances {
  implicit final val CharAlgebra = new CharAlgebra
}

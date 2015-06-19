package spire.std

import spire.algebra.{AbGroup, Order}

trait UnitOrder extends Order[Unit] with Serializable {
  override def eqv(x:Unit, y:Unit): Boolean = true
  override def neqv(x:Unit, y:Unit): Boolean = false
  override def gt(x: Unit, y: Unit): Boolean = false
  override def lt(x: Unit, y: Unit): Boolean = false
  override def gteqv(x: Unit, y: Unit): Boolean = true
  override def lteqv(x: Unit, y: Unit): Boolean = true

  override def min(x: Unit, y: Unit): Unit = {}
  override def max(x: Unit, y: Unit): Unit = {}
  def compare(x: Unit, y: Unit): Int = 0
}

trait UnitAbGroup extends AbGroup[Unit] {
  override def inverse(a: Unit): Unit = {}
  override def id: Unit = {}
  override def op(x: Unit, y: Unit): Unit = {}
}

@SerialVersionUID(0L)
class UnitAlgebra extends UnitAbGroup with UnitOrder with Serializable

trait UnitInstances {
  implicit final val UnitAlgebra = new UnitAlgebra
}

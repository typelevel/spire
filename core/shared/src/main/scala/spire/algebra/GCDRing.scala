package spire
package algebra

trait GCDRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with CRing[A] {
  def gcd(a: A, b: A): A
  def lcm(a: A, b: A): A // TODO check = times(quot(a, gcd(a, b)), b)
}

object GCDRing {
  @inline final def apply[A](implicit ev: GCDRing[A]): GCDRing[A] = ev
}

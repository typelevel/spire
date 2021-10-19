package spire
package optional

import spire.algebra.Eq

/**
 * This provides an implicit `Eq[A]` for any type `A` using Scala's (Java's) `==` (`equals`). This is generally
 * considered a bad idea, since it means you lose all type safety -- for instance, any 2 types can always be compared as
 * `Eq[Any]`.
 */
object genericEq {
  @SerialVersionUID(0L)
  private class GenericEq[@sp A] extends Eq[A] with Serializable {
    def eqv(x: A, y: A): Boolean = x == y
  }

  implicit def generic[@sp A]: Eq[A] = new GenericEq[A]
}

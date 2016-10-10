package spire
package example

import spire.algebra._
import spire.math._
import spire.implicits._

object Gcd {
  def gcd0[A:Integral](x: A, y: A): A =
    if ((x % y) === Integral[A].fromInt(0)) y else gcd0(y, x % y)

  def gcd1[A:EuclideanRing:Order](x: A, y: A): A =
    if (x % y === EuclideanRing[A].zero) y else gcd1(y, x % y)
}

object Pythagoras {
  def distance0[A:Fractional](x: A, y: A): A = (x * x + y * y).sqrt
  def distance1[A:Field:NRoot](x: A, y: A): A = (x * x + y * y).sqrt
}

package spire.algebra

import annotation.tailrec
import scala.{specialized => sp}

import spire.syntax.euclideanRing._

trait Gcd[@sp(Int, Long, Float, Double) A] extends Any {
  def gcd(a: A, b: A): A
  def lcm(a: A, b: A): A
}

object Gcd {
  @inline final def apply[A](implicit ev: Gcd[A]): Gcd[A] = ev

  @tailrec final def euclid[A: Eq: EuclideanRing](a: A, b: A): A =
    if (b.isZero) a else euclid(b, a % b)

  def fromEuclideanRing[@sp(Int, Long, Float, Double) A: EuclideanRing: Eq] =
    new Gcd[A] {
      def gcd(a: A, b: A): A = euclid(a, b)
      def lcm(a: A, b: A): A = (a /~ gcd(a, b)) * b
    }
}

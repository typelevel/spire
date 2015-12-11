package spire.algebra

import annotation.tailrec
import scala.{specialized => sp}

import spire.syntax.eq._
import spire.syntax.euclideanRing._

trait Gcd[@sp(Int, Long, Float, Double) A] extends Any {
  def gcd(a: A, b: A): A
  def lcm(a: A, b: A): A

  @tailrec final def euclid(a: A, b: A)(implicit eq: Eq[A], er: EuclideanRing[A]): A =
    if (b === er.zero) a else euclid(b, a % b)
}

object Gcd {
  @inline final def apply[A](implicit ev: Gcd[A]): Gcd[A] = ev

  implicit def fromEuclideanRing[@sp(Int, Long, Float, Double) A: EuclideanRing: Eq] =
    new Gcd[A] {
      def gcd(a: A, b: A): A = euclid(a, b)
      def lcm(a: A, b: A): A = (a /~ gcd(a, b)) * b
    }
}

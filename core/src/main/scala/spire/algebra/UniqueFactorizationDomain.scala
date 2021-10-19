/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package algebra

import spire.math.{Integral, SafeLong}

/**
 * A unique factorization domain is a commutative ring in which each element can be written as a product of prime
 * elements and a unit.
 *
 * Unique factorization domains are GCD rings (or domains), but not necessarily Euclidean domains.
 *
 * This trait is outside the commutative ring hierarchy, because the factorization algorithms are costly. Another
 * reason: in some cases, a deliberate choice should be made by the user, for example to use probabilistic algorithms
 * with a specified probability of failure.
 */
trait UniqueFactorizationDomain[@sp(Byte, Short, Int, Long) A] extends Any {

  /**
   * Tests whether the given nonzero element is prime.
   */
  def isPrime(a: A): Boolean

  /**
   * Returns the factors of the given nonzero element.
   */
  def factor(a: A): UniqueFactorizationDomain.Decomposition[A]
}

object UniqueFactorizationDomain {

  trait Decomposition[@sp(Byte, Short, Int, Long) A] {
    def unit: A
    def elements: Iterable[(A, Int)]
  }

  def apply[A](implicit ev: UniqueFactorizationDomain[A]): UniqueFactorizationDomain[A] = ev

  case class WrapDecomposition[A: CRing](safeLongFactors: spire.math.prime.Factors) extends Decomposition[A] {
    def unit: A = safeLongFactors.sign match {
      case Sign.Negative => CRing[A].negate(CRing[A].one)
      case Sign.Positive => CRing[A].one
      case _             => throw new ArithmeticException("Factorization of zero is undefined.")
    }
    override def elements: Map[A, Int] = safeLongFactors.elements.map { case (f, exp) =>
      ((CRing[A].fromBigInt(f.toBigInt), exp))
    }
  }

  implicit def uniqueFactorizationDomainFromIntegral[A](implicit A: Integral[A]): UniqueFactorizationDomain[A] =
    new UniqueFactorizationDomain[A] {
      def isPrime(a: A): Boolean = SafeLong(A.toBigInt(a)).isPrime
      def factor(a: A): Decomposition[A] = WrapDecomposition[A](SafeLong(A.toBigInt(a)).factor)(A)
    }
}

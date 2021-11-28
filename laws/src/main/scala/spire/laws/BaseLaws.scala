/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
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
package laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import scala.annotation.nowarn

import InvalidTestException._

object BaseLaws {
  def apply[A: Eq: Arbitrary] = new BaseLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait BaseLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  @nowarn
  def metricSpace[R](implicit MSA: MetricSpace[A, R], SR: Signed[R], OR: Order[R], ASR: AdditiveSemigroup[R]) =
    new SimpleRuleSet(
      name = "metricSpace",
      "non-negative" -> forAllSafe((a1: A, a2: A) => MSA.distance(a1, a2).sign != Signed.Negative),
      "identity" -> forAllSafe((a: A) => MSA.distance(a, a).sign == Signed.Zero),
      "equality" -> forAllSafe((a1: A, a2: A) =>
        // generating equal values is hard, and Scalacheck will give up if it can't
        // hence, not using `==>` here
        a1 =!= a2 || MSA.distance(a1, a2).sign == Signed.Zero
      ),
      "symmetry" -> forAllSafe((a1: A, a2: A) => MSA.distance(a1, a2) === MSA.distance(a2, a1)),
      "triangleInequality" -> forAllSafe((a1: A, a2: A, a3: A) =>
        (MSA.distance(a1, a2) + MSA.distance(a2, a3)) >= MSA.distance(a1, a3)
      )
    )

  def uniqueFactorizationDomain(implicit A: UniqueFactorizationDomain[A], RA: CRing[A]) = new SimpleRuleSet(
    name = "uniqueFactorizationDomain",
    "all factors are prime" -> forAllSafe((x: A) =>
      RA.isZero(x) || {
        val factorization = A.factor(x)
        factorization.elements.forall(pair => A.isPrime(pair._1))
      }
    ),
    "multiplying factors returns the original element" -> forAllSafe((x: A) =>
      RA.isZero(x) || {
        val factorization = A.factor(x)
        val prod = factorization.elements.map(f => RA.pow(f._1, f._2)).foldLeft(RA.one)(RA.times)
        RA.times(factorization.unit, prod) === x
      }
    )
  )

}

// vim: expandtab:ts=2:sw=2

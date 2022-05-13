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

import spire.algebra.{Bool, Eq}
import spire.algebra.lattice.Heyting
import spire.syntax.eq._
import spire.syntax.bool._

import org.typelevel.discipline.Laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

import InvalidTestException._

object LogicLaws {
  def apply[A: Eq: Arbitrary] = new LogicLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LogicLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def heyting(implicit A: Heyting[A]) =
    new DefaultRuleSet(
      name = "heyting",
      parent = None,
      "associative" -> forAllSafe { (x: A, y: A, z: A) =>
        (x & y & z) === (x & (y & z)) && (x | y | z) === (x | (y | z))
      },
      "commutative" -> forAllSafe { (x: A, y: A) =>
        (x & y) === (y & x) && (x | y) === (y | x)
      },
      "absorption" -> forAllSafe { (x: A, y: A) =>
        (x & (x | y)) === x && (x | x & y) === x
      },
      "identity" -> forAllSafe { (x: A) =>
        (x & A.one) === x && (x | A.zero) === x
      },
      "distributive" -> forAllSafe { (x: A, y: A, z: A) =>
        (x & (y | z)) === (x & y | x & z) && (x | y & z) === ((x | y) & (x | z))
      },
      "consistent" -> forAllSafe { (x: A) => (x & ~x) === A.zero },
      "¬x = (x -> 0)" -> forAllSafe { (x: A) => ~x === x.imp(A.zero) },
      "x -> x = 1" -> forAllSafe { (x: A) => x.imp(x) === A.one },
      "if x -> y and y -> x then x=y" -> forAllSafe { (x: A, y: A) =>
        x.imp(y) =!= A.one || y.imp(x) =!= A.one || x === y
      },
      "if (1 -> x)=1 then x=1" -> forAllSafe { (x: A) =>
        A.one.imp(x) =!= A.one || x === A.one
      },
      "x -> (y -> x) = 1" -> forAllSafe { (x: A, y: A) => x.imp(y.imp(x)) === A.one },
      "(x->(y->z)) -> ((x->y)->(x->z)) = 1" -> forAllSafe { (x: A, y: A, z: A) =>
        x.imp(y.imp(z)).imp(x.imp(y).imp(x.imp(z))) === A.one
      },
      "x∧y -> x = 1" -> forAllSafe { (x: A, y: A) => (x & y).imp(x) === A.one },
      "x∧y -> y = 1" -> forAllSafe { (x: A, y: A) => (x & y).imp(y) === A.one },
      "x -> y -> (x∧y) = 1" -> forAllSafe { (x: A, y: A) => x.imp(y.imp(x & y)) === A.one },
      "x -> x∨y" -> forAllSafe { (x: A, y: A) => x.imp(x | y) === A.one },
      "y -> x∨y" -> forAllSafe { (x: A, y: A) => y.imp(x | y) === A.one },
      "(x -> z) -> ((y -> z) -> ((x | y) -> z)) = 1" -> forAllSafe { (x: A, y: A, z: A) =>
        x.imp(z).imp(y.imp(z).imp((x | y).imp(z))) === A.one
      },
      "(0 -> x) = 1" -> forAllSafe { (x: A) => A.zero.imp(x) === A.one }
    )

  def bool(implicit A: Bool[A]) =
    new DefaultRuleSet(name = "bool",
                       parent = Some(heyting),
                       "excluded middle" -> forAllSafe { (x: A) => (x | ~x) === A.one },
                       "xor" -> forAllSafe { (a: A, b: A) => (a ^ b) === (a & ~b | ~a & b) },
                       "nxor" -> forAllSafe { (a: A, b: A) => a.nxor(b) === ((a | ~b) & (~a | b)) },
                       "imp" -> forAllSafe { (a: A, b: A) => a.imp(b) === (~a | b) },
                       "nand" -> forAllSafe { (a: A, b: A) => a.nand(b) === ~(a & b) },
                       "nor" -> forAllSafe { (a: A, b: A) => a.nor(b) === ~(a | b) }
    )
}

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

package spire.laws.shadows

import spire.algebra.IsIntegral
import spire.laws.InvalidTestException
import spire.math.NumberTag

object Shadowing {
  def apply[A, S](f: A => S, g: S => Option[A]): Shadowing[A, S] = new Shadowing[A, S] {
    def toShadow(a: A): S = f(a)
    def fromShadow(s: S): Option[A] = g(s)
  }

  def bigInt[A: IsIntegral: NumberTag](fromBigInt: BigInt => A): Shadowing[A, BigInt] =
    new Shadowing[A, BigInt] {
      def toShadow(a: A): BigInt = IsIntegral[A].toBigInt(a)
      def fromShadow(s: BigInt): Option[A] = {
        NumberTag[A].hasMinValue match {
          case Some(m) if s < IsIntegral[A].toBigInt(m) => return None
          case _                                        =>
        }
        NumberTag[A].hasMaxValue match {
          case Some(m) if s > IsIntegral[A].toBigInt(m) => return None
          case _                                        =>
        }
        Some(fromBigInt(s))
      }
    }
}

trait Shadowing[A, S] {
  def toShadow(a: A): S
  def fromShadow(s: S): Option[A]
  def isValid(s: S): Boolean = fromShadow(s).nonEmpty
  def checked(s: S): S =
    if (!isValid(s)) throw new InvalidTestException else s
}

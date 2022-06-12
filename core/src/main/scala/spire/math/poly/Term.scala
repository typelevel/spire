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
package math
package poly

import spire.algebra.{Eq, Field, Order, Rig, Ring, Rng, Semiring}
import spire.syntax.field._
import spire.syntax.eq._

// Univariate polynomial term
case class Term[@sp(Float, Double) C](coeff: C, exp: Int) { lhs =>

  def unary_-(implicit r: Rng[C]): Term[C] = Term(-coeff, exp)

  def +(rhs: Term[C])(implicit r: Semiring[C]): Term[C] = {
    if (lhs.exp != rhs.exp)
      throw new IllegalArgumentException(s"can't add terms of degree $exp and ${rhs.exp}")
    Term(lhs.coeff + rhs.coeff, lhs.exp)
  }

  def *(rhs: Term[C])(implicit r: Semiring[C]): Term[C] =
    Term(lhs.coeff * rhs.coeff, lhs.exp + rhs.exp)

  def toTuple: (Int, C) = (exp, coeff)

  def eval(x: C)(implicit r: Semiring[C]): C =
    if (exp != 0) coeff * (x.pow(exp)) else coeff

  def isIndexZero: Boolean =
    exp == 0

  def isZero(implicit ring: Semiring[C], eq: Eq[C]): Boolean =
    coeff === ring.zero

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der(implicit r: Ring[C]): Term[C] =
    Term(coeff * r.fromInt(exp), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt(exp + 1), exp + 1)

  override def toString: String = {
    import Term._

    def expString = exp match {
      case 0 => ""
      case 1 => "x"
      case _ => "x" + exp.toString.map(superscript)
    }

    def simpleCoeff: Option[String] = coeff match {
      case 0              => Some("")
      case 1 if exp == 0  => Some(s" + $coeff")
      case 1              => Some(s" + $expString")
      case -1 if exp != 0 => Some(s" - $expString")
      case _              => None
    }

    def stringCoeff: Option[String] = coeff.toString match {
      case IsZero()                        => Some("")
      case IsNegative(posPart) if exp == 0 => Some(s" - $posPart")
      case IsNegative(posPart)             => Some(s" - $posPart$expString")
      case _                               => None
    }

    simpleCoeff.orElse(stringCoeff).getOrElse(s" + $coeff$expString")
  }
}

object Term {
  implicit def ordering[C]: Order[Term[C]] = new Order[Term[C]] {
    def compare(x: Term[C], y: Term[C]): Int = x.exp.compare(y.exp)
  }

  def fromTuple[@sp(Float, Double) C](tpl: (Int, C)): Term[C] =
    Term(tpl._2, tpl._1)
  def zero[@sp(Float, Double) C](implicit r: Semiring[C]): Term[C] =
    Term(r.zero, 0)
  def one[@sp(Float, Double) C](implicit r: Rig[C]): Term[C] =
    Term(r.one, 0)

  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r

  private val digitToSuperscript = Array(
    ('0', '\u2070'),
    ('1', '\u2071'),
    ('2', '\u2072'),
    ('3', '\u2073'),
    ('4', '\u2074'),
    ('5', '\u2075'),
    ('6', '\u2076'),
    ('7', '\u2077'),
    ('8', '\u2078'),
    ('9', '\u2079'),
    ('-', '\u207B'),
    ('1', '\u00B9'),
    ('2', '\u00B2'),
    ('3', '\u00B3')
  )

  // call Regex constructor directly to get rid of compiler warning
  // replace with "".r once SI-6723 is fixed
  private val superscriptRegex =
    if (Platform.isNative) null
    else
      new scala.util.matching.Regex(
        "[\\u2070\\u2071\\u2072\\u2073\\u2074\\u2075\\u2076\\u2077\\u2078\\u2079\\u207B\\u00B9\\u00B2\\u00B3]+"
      )

  private[spire] def removeSuperscript(text: String): String =
    if (Platform.isNative) {
      var i = 0
      val n = text.length
      var sup = false
      val sb = new scala.collection.mutable.StringBuilder(text.length + text.length / 2)
      while (i < n) {
        val c = text(i)
        removeSuperscript
          .get(c)
          .fold {
            sup = false
            sb += c
          } { d =>
            if (!sup) sb += '^'
            sup = true
            sb += d
          }
        i += 1
      }
      if (sb.length() != n) sb.result() else text
    } else
      superscriptRegex.replaceAllIn(text, "^" + _.group(0).map(removeSuperscript))

  private val superscript: (Char => Char) = Map(digitToSuperscript.toIndexedSeq: _*)

  private val removeSuperscript = Map(digitToSuperscript.map(_.swap).toIndexedSeq: _*)
}

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
package syntax

import spire.algebra._
import spire.algebra.partial._
import spire.math._

final class LiteralIntOrderOps(val lhs: Int) extends AnyVal {
  def <[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lt(c.fromInt(lhs), rhs)
  def <=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lteqv(c.fromInt(lhs), rhs)
  def >[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gt(c.fromInt(lhs), rhs)
  def >=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gteqv(c.fromInt(lhs), rhs)

  def cmp[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Int = ev.compare(c.fromInt(lhs), rhs)
  def min[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.min(c.fromInt(lhs), rhs)
  def max[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.max(c.fromInt(lhs), rhs)
}

final class LiteralLongOrderOps(val lhs: Long) extends AnyVal {
  def <[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lt(c.fromLong(lhs), rhs)
  def <=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lteqv(c.fromLong(lhs), rhs)
  def >[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gt(c.fromLong(lhs), rhs)
  def >=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gteqv(c.fromLong(lhs), rhs)

  def cmp[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Int = ev.compare(c.fromLong(lhs), rhs)
  def min[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.min(c.fromLong(lhs), rhs)
  def max[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.max(c.fromLong(lhs), rhs)
}

final class LiteralDoubleOrderOps(val lhs: Double) extends AnyVal {
  def <[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lt(c.fromDouble(lhs), rhs)
  def <=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lteqv(c.fromDouble(lhs), rhs)
  def >[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gt(c.fromDouble(lhs), rhs)
  def >=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gteqv(c.fromDouble(lhs), rhs)

  def cmp[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Int = ev.compare(c.fromDouble(lhs), rhs)
  def min[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.min(c.fromDouble(lhs), rhs)
  def max[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.max(c.fromDouble(lhs), rhs)
}

final class LiteralIntTruncatedDivisionOps(val lhs: Int) extends AnyVal {
  def tquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tquot(c.fromInt(lhs), rhs)
  def tmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tmod(c.fromInt(lhs), rhs)
  def tquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.tquotmod(c.fromInt(lhs), rhs)
  def fquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fquot(c.fromInt(lhs), rhs)
  def fmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fmod(c.fromInt(lhs), rhs)
  def fquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.fquotmod(c.fromInt(lhs), rhs)
}

final class LiteralLongTruncatedDivisionOps(val lhs: Long) extends AnyVal {
  def tquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tquot(c.fromLong(lhs), rhs)
  def tmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tmod(c.fromLong(lhs), rhs)
  def tquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.tquotmod(c.fromLong(lhs), rhs)
  def fquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fquot(c.fromLong(lhs), rhs)
  def fmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fmod(c.fromLong(lhs), rhs)
  def fquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.fquotmod(c.fromLong(lhs), rhs)
}

final class LiteralDoubleTruncatedDivisionOps(val lhs: Double) extends AnyVal {
  def tquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tquot(c.fromDouble(lhs), rhs)
  def tmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tmod(c.fromDouble(lhs), rhs)
  def tquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.tquotmod(c.fromDouble(lhs), rhs)
  def fquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fquot(c.fromDouble(lhs), rhs)
  def fmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fmod(c.fromDouble(lhs), rhs)
  def fquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.fquotmod(c.fromDouble(lhs), rhs)
}

final class GroupoidCommonOps[A](lhs: A)(implicit ev: Groupoid[A]) {
  def inverse: A = ev.inverse(lhs)
  def isId(implicit ev1: Eq[A]): Boolean = ev.isId(lhs)(ev1)
}

final class LiteralIntAdditiveSemigroupOps(val lhs: Int) extends AnyVal {
  def +[A](rhs: A)(implicit ev: Ring[A]): A = ev.plus(ev.fromInt(lhs), rhs)
}

final class LiteralLongAdditiveSemigroupOps(val lhs: Long) extends AnyVal {
  def +[A](rhs: A)(implicit ev: Ring[A], c: ConvertableTo[A]): A = ev.plus(c.fromLong(lhs), rhs)
}

final class LiteralDoubleAdditiveSemigroupOps(val lhs: Double) extends AnyVal {
  def +[A](rhs: A)(implicit ev: Field[A]): A = ev.plus(ev.fromDouble(lhs), rhs)
}

final class LiteralIntAdditiveGroupOps(val lhs: Int) extends AnyVal {
  def -[A](rhs: A)(implicit ev: Ring[A]): A = ev.minus(ev.fromInt(lhs), rhs)
}

final class LiteralLongAdditiveGroupOps(val lhs: Long) extends AnyVal {
  def -[A](rhs: A)(implicit ev: Ring[A], c: ConvertableTo[A]): A = ev.minus(c.fromLong(lhs), rhs)
}

final class LiteralDoubleAdditiveGroupOps(val lhs: Double) extends AnyVal {
  def -[A](rhs: A)(implicit ev: Field[A]): A = ev.minus(ev.fromDouble(lhs), rhs)
}

final class LiteralIntMultiplicativeSemigroupOps(val lhs: Int) extends AnyVal {
  def *[A](rhs: A)(implicit ev: Ring[A]): A = ev.times(ev.fromInt(lhs), rhs)
}

final class LiteralLongMultiplicativeSemigroupOps(val lhs: Long) extends AnyVal {
  def *[A](rhs: A)(implicit ev: Ring[A], c: ConvertableTo[A]): A = ev.times(c.fromLong(lhs), rhs)
}

final class LiteralDoubleMultiplicativeSemigroupOps(val lhs: Double) extends AnyVal {
  def *[A](rhs: A)(implicit ev: Field[A]): A = ev.times(ev.fromDouble(lhs), rhs)
}

final class LiteralIntMultiplicativeGroupOps(val lhs: Int) extends AnyVal {
  def /[A](rhs: A)(implicit ev: Field[A]): A = ev.div(ev.fromInt(lhs), rhs)
}

final class LiteralLongMultiplicativeGroupOps(val lhs: Long) extends AnyVal {
  def /[A](rhs: A)(implicit ev: Field[A], c: ConvertableTo[A]): A = ev.div(c.fromLong(lhs), rhs)
}

final class LiteralDoubleMultiplicativeGroupOps(val lhs: Double) extends AnyVal {
  def /[A](rhs: A)(implicit ev: Field[A]): A = ev.div(ev.fromDouble(lhs), rhs)
}

final class LiteralIntEuclideanRingOps(val lhs: Int) extends AnyVal {
  def equot[A](rhs: A)(implicit ev: EuclideanRing[A]): A = ev.equot(ev.fromInt(lhs), rhs)
  def emod[A](rhs: A)(implicit ev: EuclideanRing[A]): A = ev.emod(ev.fromInt(lhs), rhs)
  def equotmod[A](rhs: A)(implicit ev: EuclideanRing[A]): (A, A) = ev.equotmod(ev.fromInt(lhs), rhs)
}

final class LiteralLongEuclideanRingOps(val lhs: Long) extends AnyVal {
  def equot[A](rhs: A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]): A = ev.equot(c.fromLong(lhs), rhs)
  def emod[A](rhs: A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]): A = ev.emod(c.fromLong(lhs), rhs)
  def equotmod[A](rhs: A)(implicit ev: EuclideanRing[A], c: ConvertableTo[A]): (A, A) =
    ev.equotmod(c.fromLong(lhs), rhs)
}

final class LiteralDoubleEuclideanRingOps(val lhs: Double) extends AnyVal {
  def equot[A](rhs: A)(implicit ev: Field[A]): A = ev.equot(ev.fromDouble(lhs), rhs)
  def emod[A](rhs: A)(implicit ev: Field[A]): A = ev.emod(ev.fromDouble(lhs), rhs)
  def equotmod[A](rhs: A)(implicit ev: Field[A]): (A, A) = ev.equotmod(ev.fromDouble(lhs), rhs)
}

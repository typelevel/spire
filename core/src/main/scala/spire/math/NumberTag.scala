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
package math

object NumberTag {

  def apply[A](implicit ev: NumberTag[A]): NumberTag[A] = ev

  sealed trait Resolution
  case object Integral extends Resolution
  case object Approximate extends Resolution
  case object Exact extends Resolution

  class BuiltinIntTag[A](zero: A, min: A, max: A) extends NumberTag[A] {
    def resolution: Resolution = Integral

    val hasZero: Option[A] = Some(zero)
    val hasMinValue: Option[A] = Some(min)
    val hasMaxValue: Option[A] = Some(max)

    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None

    def overflows: Boolean = true
    def isSigned: Boolean = true

    def isInfinite(a: A): Boolean = false
    def isNaN(a: A): Boolean = false
  }

  class UnsignedIntTag[A](zero: A, max: A) extends NumberTag[A] {
    def resolution: Resolution = Integral

    val hasZero: Option[A] = Some(zero)
    val hasMinValue: Option[A] = Some(zero)
    val hasMaxValue: Option[A] = Some(max)

    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None

    def overflows: Boolean = true
    def isSigned: Boolean = false

    def isInfinite(a: A): Boolean = false
    def isNaN(a: A): Boolean = false
  }

  abstract class BuiltinFloatTag[A](zero: A, min: A, max: A, nan: A, posInf: A, negInf: A) extends NumberTag[A] {
    def resolution: Resolution = Approximate

    val hasZero: Option[A] = Some(zero)
    val hasMinValue: Option[A] = Some(min)
    val hasMaxValue: Option[A] = Some(max)

    val hasNaN: Option[A] = Some(nan)
    val hasPositiveInfinity: Option[A] = Some(posInf)
    val hasNegativeInfinity: Option[A] = Some(negInf)

    def overflows: Boolean = false
    def isSigned: Boolean = true
  }

  class LargeTag[A](val resolution: Resolution, zero: A) extends NumberTag[A] {
    val hasZero: Option[A] = Some(zero)
    def hasMinValue: Option[A] = None
    def hasMaxValue: Option[A] = None

    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None

    def overflows: Boolean = false
    def isSigned: Boolean = true

    def isInfinite(a: A): Boolean = false
    def isNaN(a: A): Boolean = false
  }

  class CustomTag[A](val resolution: Resolution,
                     val hasZero: Option[A],
                     val hasMinValue: Option[A],
                     val hasMaxValue: Option[A],
                     val overflows: Boolean,
                     val isSigned: Boolean
  ) extends NumberTag[A] {
    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None
    def isInfinite(a: A): Boolean = false
    def isNaN(a: A): Boolean = false
  }
}

/**
 * A `NumberTag` provides information about important implementations details of numbers. For instance, it includes
 * information about whether we can expect arithmetic to overflow or produce invalid values, the bounds of the number if
 * they exist, whether it is an approximate or exact number type, etc.
 */
trait NumberTag[A] {

  /**
   * Returns the resolution of this number.
   */
  def resolution: NumberTag.Resolution

  /**
   * Returns the smallest finite value that `A` can represent, if one exists. For instance, the smallest finite value
   * representable by `Double` is `-1.7976931348623157E308`. On the other hand, `BigInt` has no smallest value.
   */
  def hasMinValue: Option[A]

  /**
   * If `A` has a value that represents the real value 0, then it is returned here. Otherwise `None` is returned.
   */
  def hasZero: Option[A]

  /**
   * Returns the largest finite value that `A` can represent, if one exists. For instance, the largest finite value
   * representable by `Double` is `1.7976931348623157E308`. On the other hand, `BigInt` has no largest value.
   */
  def hasMaxValue: Option[A]

  /**
   * If `A` has values that represent an undefined or invalid value, then a repsentitive value may be used here.
   * Otherwise this returned `None` to indicate that all values in `A` are valid numbers in the extended real number
   * line.
   */
  def hasNaN: Option[A]

  /**
   * If `A` has a value that represents a positive infinity, then it is returned here, otherwise a value of `None`
   * indicates that positive infinity cannot be represented in `A`.
   */
  def hasPositiveInfinity: Option[A]

  /**
   * If `A` has a value that represents a negative infinity, then it is returned here, otherwise a value of `None`
   * indicates that negative infinity cannot be represented in `A`.
   */
  def hasNegativeInfinity: Option[A]

  /**
   * Returns true if this value can overflow as a result of arithmetic operations. Types that overflow include `Int` and
   * `Long`.
   */
  def overflows: Boolean

  /**
   * Returns true if `A` can represent both positive and negative values.
   */
  def isSigned: Boolean

  /**
   * Returns true if all values representable by `A` are finite and live on the real number line.
   */
  def finite: Boolean =
    hasMinValue.isDefined && hasMaxValue.isDefined

  /**
   * Returns true if this type can represent arbitrarily large or small values.
   */
  def infinite: Boolean =
    hasMinValue.isEmpty || hasMaxValue.isEmpty

  /**
   * Returns `true` if `a` is an infinite value (either positive or negative) and false otherwise.
   */
  def isInfinite(a: A): Boolean

  /**
   * Returns `true` if `a` is an invalid number. Note that positive and negative infinities are valid numbers.
   */
  def isNaN(a: A): Boolean

  /**
   * Returns `true` if `a` represents a finite value (neither infinite nor invalid).
   */
  def isFinite(a: A): Boolean = !(isInfinite(a) || isNaN(a))
}

package spire.math

object Info {

  def apply[A](implicit ev: Info[A]): Info[A] = ev

  sealed trait Resolution
  case object Integral extends Resolution
  case object Approximate extends Resolution
  case object Exact extends Resolution

  class BuiltinIntInfo[A](zero: A, min: A, max: A) extends Info[A] {
    def resolution: Resolution = Integral

    val hasZero: Option[A] = Some(zero)
    val hasMinValue: Option[A] = Some(min)
    val hasMaxValue: Option[A] = Some(max)

    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None

    def overflows: Boolean = true
    def isSigned: Boolean = true
  }

  class UnsignedIntInfo[A](zero: A, max: A) extends Info[A] {
    def resolution: Resolution = Integral

    val hasZero: Option[A] = Some(zero)
    val hasMinValue: Option[A] = Some(zero)
    val hasMaxValue: Option[A] = Some(max)

    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None

    def overflows: Boolean = true
    def isSigned: Boolean = false
  }

  class BuiltinFloatInfo[A](zero: A, min: A, max: A, nan: A, posInf: A, negInf: A) extends Info[A] {
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

  class LargeInfo[A](val resolution: Resolution, zero: A) extends Info[A] {
    val hasZero: Option[A] = Some(zero)
    def hasMinValue: Option[A] = None
    def hasMaxValue: Option[A] = None

    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None

    def overflows: Boolean = false
    def isSigned: Boolean = true
  }

  class CustomInfo[A](val resolution: Resolution,
    val hasZero: Option[A], val hasMinValue: Option[A],
    val hasMaxValue: Option[A], val overflows: Boolean,
    val isSigned: Boolean) extends Info[A] {
    def hasNaN: Option[A] = None
    def hasPositiveInfinity: Option[A] = None
    def hasNegativeInfinity: Option[A] = None
  }
}

trait Info[A] {
  def resolution: Info.Resolution

  def hasMinValue: Option[A]
  def hasZero: Option[A]
  def hasMaxValue: Option[A]

  def hasNaN: Option[A]
  def hasPositiveInfinity: Option[A]
  def hasNegativeInfinity: Option[A]

  def overflows: Boolean
  def isSigned: Boolean

  def isFinite: Boolean =
    hasMinValue.isDefined && hasMaxValue.isDefined

  def isInfinite: Boolean =
    hasMinValue.isEmpty || hasMaxValue.isEmpty
}

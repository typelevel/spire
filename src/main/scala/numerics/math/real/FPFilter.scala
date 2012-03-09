package numerics.math.real

import numerics.math.fpf.MaybeDouble
import numerics.math._


/**
 * A mix-in for `RealLike` that adds an internal floating pointer filter.
 */
trait FPFilter[A <: FPFilter[A]] extends RealLike[A] { self: A =>
  lazy val fpf: MaybeDouble = this match {
    case Add(a, b) => a.fpf + b.fpf
    case Sub(a, b) => a.fpf - b.fpf
    case Mul(a, b) => a.fpf * b.fpf
    case Div(a, b) => a.fpf / b.fpf
    case KRoot(a, k) => a.fpf nroot k
    case Neg(a) => -(a.fpf)
    case IntLit(n) => MaybeDouble(n)
    case BigIntLit(n) => MaybeDouble(n)
  }

  abstract override lazy val sign: Sign = fpf.sign getOrElse super.sign
  abstract override def toBigInt: BigInt = fpf.toLong map (BigInt(_)) getOrElse super.toBigInt
  abstract override def isWhole: Boolean = fpf.isWhole getOrElse super.isWhole
  abstract override def doubleValue: Double = if (fpf.isExact) fpf.approx else super.doubleValue
  abstract override def floatValue: Float = fpf.toFloat getOrElse super.floatValue
  abstract override def intValue: Int = fpf.toLong map (_.toInt) getOrElse super.intValue 
  abstract override def longValue: Long = fpf.toLong getOrElse super.longValue
}



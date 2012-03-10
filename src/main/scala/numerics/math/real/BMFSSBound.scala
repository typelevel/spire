package numerics.math.real

import numerics.math._
import Implicits._


/**
 * A mixin for the SeparationBound that implements the BMFSS bound.
 */
trait BMFSSBound[A <: BMFSSBound[A]] extends SeparationBound[A] { self: A =>
  
  private lazy val l: BigInt = this match {
    case IntLit(_) | BigIntLit(_) => BigInt(1)
    case KRoot(a, k) => if (a.u < a.l) (((a.u pow (k - 1)) * a.l) nroot k) + 1 else a.l
    case Add(a, b) => a.l * b.l
    case Sub(a, b) => a.l * b.l
    case Mul(a, b) => a.l * b.l
    case Div(a, b) => a.l * b.u
    case Neg(a) => a.l
  }

  private lazy val u: BigInt = this match {
    case IntLit(n) => BigInt(n).abs
    case BigIntLit(n) => n.abs
    case KRoot(a, k) => if (a.u >= a.l) ((a.u * (a.l pow (k - 1))) nroot k) + 1 else a.u
    case Add(a, b) => a.u * b.l + a.l * b.u
    case Sub(a, b) => a.u * b.l + a.l * b.u
    case Mul(a, b) => a.u * b.u
    case Div(a, b) => a.u * b.l
    case Neg(a) => a.u
  }

  private lazy val weight: Int = this match {
    case IntLit(_) | BigIntLit(_) => 1
    case Neg(a) => a.weight
    case KRoot(a, k) => k * a.weight
    case Add(a, b) => a.weight * b.weight
    case Sub(a, b) => a.weight * b.weight
    case Mul(a, b) => a.weight * b.weight
    case Div(a, b) => a.weight * b.weight
  }

  def lowerBound: Int = {
    val d = (u pow (weight - 1)) * l
    if (d == 1) 0 else -d.bitLength
  }

  def upperBound: Int = ((l pow (weight - 1)) * u).bitLength
}


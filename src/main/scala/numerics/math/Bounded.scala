package numerics.math


/** 
 * A `Bounded` `Real` provides a way of obtaining lower and upper bounds on
 * the absolute value of the underlying `Real` (x). More importantly, these
 * are zero bound functions, s.t. if x != 0, then 
 * `2^lowerBound <= |x| <= 2^upperBound`.
 *
 * TODO: Abstract this so other zero bounds can be used, or aggregated.
 */
sealed trait Bounded {
  protected[math] def l: BigInt 
  protected[math] def u: BigInt
  protected[math] def weight: Int

  /**
   * This returns an `Int` `k` s.t. 2^`k` <= the value this is bounding.
   *
   * TODO: We could provide a slightly better bound if we check if 
   * 2^bitLength - 1  == (u pow (w - 1) * l), but I'm not sure it's worth it.
   */
  def lowerBound: Int = {
    val d = (u pow (weight - 1)) * l
    if (d == 1) 0 else -d.bitLength
  }

  /**
   * This returns an int `k` s.t. 2^`k` >= the value this is bounding.
   */
  def upperBound: Int = ((l pow (weight - 1)) * u).bitLength

  // Rough approximations to upperBound * math.ceil(math.log10(2)) and
  // lowerBound * math.floor(math.log10(2)).

  def decimalUpperBound: Int = (upperBound * 4 + 12) / 13
  def decimalLowerBound: Int = (lowerBound * 3 - 9) / 10
}

object Bounded extends (Real => Bounded) {
  implicit def apply(num: Real): Bounded = num match {
    case Add(lhs, rhs) => BoundedAdd(this(lhs), this(rhs))
    case Sub(lhs, rhs) => BoundedAdd(this(lhs), this(rhs))
    case Mul(lhs, rhs) => BoundedMul(this(lhs), this(rhs))
    case Div(lhs, rhs) => BoundedDiv(this(lhs), this(rhs))
    case Neg(x) => this(x)
    case KRoot(x, k) => BoundedKRoot(this(x), k)
    case IntLit(n) => BoundedIntLit(n)
    case BigIntLit(n) => BoundedBigIntLit(n)
  }
}

case class BoundedIntLit(n: Int) extends Bounded {
  lazy val l = BigInt(1)
  lazy val u = if (n < 0) BigInt(-n) else BigInt(n)
  val weight: Int = 1
}

case class BoundedBigIntLit(n: BigInt) extends Bounded {
  lazy val l = BigInt(1)
  lazy val u = n.abs
  val weight: Int = 1
}

// A mixin for binary operations.
sealed trait BoundedBinOp extends Bounded {
  def lhs: Bounded
  def rhs: Bounded
  lazy val weight: Int = lhs.weight * rhs.weight
}

case class BoundedAdd(lhs: Bounded, rhs: Bounded) extends BoundedBinOp {
  lazy val l = lhs.l * rhs.l
  lazy val u = lhs.u * rhs.l + lhs.l * rhs.u
}

case class BoundedMul(lhs: Bounded, rhs: Bounded) extends BoundedBinOp {
  lazy val l = lhs.l * rhs.l
  lazy val u = lhs.u * rhs.u
}

case class BoundedDiv(lhs: Bounded, rhs: Bounded) extends BoundedBinOp {
  lazy val l = lhs.l * rhs.u
  lazy val u = lhs.u * rhs.l
}

case class BoundedKRoot(b: Bounded, k: Int) extends Bounded {
  import Implicits._

  lazy val l = if (b.u >= b.l) {
    b.l
  } else {
    (((b.u pow (k - 1)) * b.l) nroot k) + 1
  }

  lazy val u = if (b.u >= b.l) {
    ((b.u * (b.l pow (k - 1))) nroot k) + 1
  } else {
    b.u
  }

  lazy val weight: Int = k * b.weight
}




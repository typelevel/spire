package numerics.math.real

import numerics.math._


/**
 * This folds all ring ops (+, -, *) on constants to constants. This includes
 * `BigIntLit`s, which may not be desirable (ie. slow).
 */
trait ConstantFolder extends RealTransform { self: Real =>

  private def wrap(n: Long): Real = if (n > Int.MaxValue || n < Int.MinValue) {
    BigIntLit(BigInt(n))
  } else {
    IntLit(n.toInt)
  }

  private def wrap(n: BigInt): Real =
    if (n.isValidInt) IntLit(n.toInt) else BigIntLit(n)

  override def transform(num: Real): Real = { println("."); super.transform(num) match {
    case Add(IntLit(a), IntLit(b)) => wrap((a: Long) + (b: Long))
    case Add(IntLit(a), BigIntLit(b)) => wrap(b + a)
    case Add(BigIntLit(a), IntLit(b)) => wrap(a + b)
    case Add(BigIntLit(a), BigIntLit(b)) => wrap(a + b)
    case Sub(IntLit(a), IntLit(b)) => wrap((a: Long) - (b: Long))
    case Sub(IntLit(a), BigIntLit(b)) => wrap(BigInt(a) - b)
    case Sub(BigIntLit(a), IntLit(b)) => wrap(a - b)
    case Sub(BigIntLit(a), BigIntLit(b)) => wrap(a - b)
    case Mul(IntLit(a), IntLit(b)) => wrap((a: Long) * (b: Long))
    case Mul(IntLit(a), BigIntLit(b)) => wrap(b * a)
    case Mul(BigIntLit(a), IntLit(b)) => wrap(a * b)
    case Mul(BigIntLit(a), BigIntLit(b)) => wrap(a * b)
    case _ => num
  }}
}


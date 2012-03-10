package numerics.math.real


/**
 * This folds all ring ops (+, -, *) on constants to constants. This only works
 * on `IntLit`s for now, as `BigInt`s may be slow.
 *
 * TODO: It may be worth it to fold `BigIntLit`s for add/subtract, since the
 * space requirement would approximately half.
 */
trait ConstantFolder[A <: RealLike[A]] extends RealTransform[A] { self: A =>

  private def wrap(n: Long): A = if (n > Int.MaxValue || n < Int.MinValue) {
    BigIntLit(BigInt(n))
  } else {
    IntLit(n.toInt)
  }

  private def wrap(n: BigInt): A =
    if (n.isValidInt) IntLit(n.toInt) else BigIntLit(n)

  override def transform(num: A): A = super.transform(num) match {
    case Add(IntLit(a), IntLit(b)) => wrap((a: Long) + (b: Long))
    case Sub(IntLit(a), IntLit(b)) => wrap((a: Long) - (b: Long))
    case Mul(IntLit(a), IntLit(b)) => wrap((a: Long) * (b: Long))
    case num => num
  }
}


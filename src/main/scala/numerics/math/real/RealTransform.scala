package numerics.math.real

import numerics.math._


/**
 * A mixin for `Real` that let's us transform the tree as its being built.
 * In your implementation of `transform`, you'll likely want to play nice and
 * call `super.transform(x)` first.
 */
trait RealTransform extends RealLike { self: Real =>
  /**
   * Override this to return the `Real` resulting from the tree.
   */
  def transform(x: Real): Real = x

  override def *(that: Real): Real = transform(super.*(that))
  override def +(that: Real): Real = transform(super.+(that))
  override def -(that: Real): Real = transform(super.-(that))
  override def /(that: Real): Real = transform(super./(that))
  override def unary_-(): Real = transform(super.unary_-())
  override def nroot(k: Int): Real = transform(super.nroot(k))
}


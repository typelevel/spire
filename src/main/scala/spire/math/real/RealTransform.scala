package spire.math.real

import spire.math._


/**
 * A mixin for a `Real` that let's us transform the tree as its being built.
 * In your implementation of `transform`, you'll likely want to play nice and
 * call `super.transform(x)` first.
 */
trait RealTransform[A <: RealLike[A]] extends RealLike[A] { self: A =>
  /**
   * Override this to return the `Real` resulting from the tree.
   */
  def transform(a: A): A = a

  override def +(that: A): A = transform(super.+(that))
  override def -(that: A): A = transform(super.-(that))
  override def *(that: A): A = transform(super.*(that))
  override def /(that: A): A = transform(super./(that))
  override def nroot(k: Int): A = transform(super.nroot(k)) 
  override def unary_-(): A = transform(super.unary_-())
}


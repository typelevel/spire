package spire.math.extras.interval

import spire.math.Interval

abstract class IntervalSet[T, S <: IntervalSet[T, _]] extends (T => Boolean) {

  def isEmpty: Boolean

  def isContiguous: Boolean

  def hull: Interval[T]

  def at(value: T): Boolean

  def above(value: T): Boolean

  def below(value: T): Boolean

  def belowAll: Boolean

  def aboveAll: Boolean

  def |(rhs: S): S

  def &(rhs: S): S

  def ^(rhs: S): S

  def unary_~ : S

  def intersects(rhs: S): Boolean

  def isSupersetOf(rhs: S): Boolean

  def isProperSupersetOf(rhs: S): Boolean

  def intervals: Traversable[Interval[T]]

  def edges: Iterable[T]

  def intervalIterator: Iterator[Interval[T]]
}

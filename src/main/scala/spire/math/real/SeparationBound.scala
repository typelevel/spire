package spire.math.real


/**
 * A `SeparationBound` provides a way to bound a real number s.t. if the number
 * is not 0, then its absolute value is >= 2^`lowerBound`.
 */
trait SeparationBound[A <: RealLike[A]] extends RealLike[A] { self: A =>
  /**
   * This returns an `Int` `k` s.t. 2^`k` <= the value this is bounding.
   */
  def lowerBound: Int

  /**
   * This returns an int `k` s.t. 2^`k` >= the value this is bounding.
   */
  def upperBound: Int

  // Rough approximations to upperBound * math.ceil(math.log10(2)) and
  // lowerBound * math.floor(math.log10(2)).

  def decimalUpperBound: Int = (upperBound * 4 + 12) / 13
  def decimalLowerBound: Int = (lowerBound * 3 - 9) / 10
}


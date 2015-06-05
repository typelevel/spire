package spire.stats.distribution

import spire.random.Generator

trait Distribution[A] {

  /** Function used to transform the values drawn from a Generator according to this probability distribution function
    */
  private[stats] val transform: (Generator => A)
}

trait ContinuousDistribution[A] extends Distribution[A] {

  /** Returns the probability density function (pdf) of this distribution at the value x
    */
  def pdf(x: A): A

  /** Returns the cumulative distribution function (cdf) of this distribution at the value x
    */
  def cdf(x: A): A
}

trait DiscreteDistribution[A] extends Distribution[A] {

  /** Returns the probability mass function (pmf) of this distribution at the value x
    */
  def pmf(x: A): A

  /** Returns the cumulative distribution function (cdf) of this distribution at the value x
    */
  def cdf(x: A): A
}


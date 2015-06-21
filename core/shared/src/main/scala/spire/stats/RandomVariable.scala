package spire.stats

import scala.reflect.ClassTag
import spire.random.Generator
import spire.stats.distribution.Distribution

sealed abstract class RandomVariable[A] {

  /** Generates a random number according to this variable's probability distribution function
    */
  def observe(gen: Generator): A

  /** Generates an array of random numbers according to this variable's probability distribution function
    */
  def observeMany(gen: Generator, count: Int)(implicit ev: ClassTag[A]): Array[A] = {
    val arr = Array.ofDim[A](count)
    fill(gen, arr)
    
    arr
  }

  /** Fills an array of random numbers according to this variable's probability distribution function
    */
  def fill(gen: Generator, arr: Array[A]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = observe(gen)
      i += 1
    }
  }
}

object RandomVariable {
  def apply[A, B](distribution: Distribution[A, B]) = new RandomVariableWithDistribution(distribution)
}

class RandomVariableWithDistribution[A, B](distribution: Distribution[A, B]) extends RandomVariable[A] {
  def observe(gen: Generator) = distribution.transform(gen)
}


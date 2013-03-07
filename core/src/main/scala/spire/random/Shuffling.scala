package spire.random

import scala.{specialized => spec}
import scala.reflect.ClassTag

object Shuffling {
  def shuffle[@spec A](as: Array[A])(implicit gen: Generator) {
    var i: Int = as.length - 1
    while (i > 0) {
      val n: Int = gen.nextInt(i)
      val tmp: A = as(i)
      as(i) = as(n)
      as(n) = tmp
      i -= 1
    }
  }
}

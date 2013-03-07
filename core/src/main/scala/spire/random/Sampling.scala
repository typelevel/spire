package spire.random

import scala.{specialized => spec}
import scala.reflect.ClassTag

object Sampling {
  def chooseFromArray[@spec A](arr: Array[A])(implicit gen: Generator): A =
    arr(gen.nextInt(arr.length))

  def chooseFromSeq[A](seq: Seq[A])(implicit gen: Generator): A =
    seq(gen.nextInt(seq.length))

  def chooseFromIterable[A](as: Iterable[A])(implicit gen: Generator): A =
    as.iterator.drop(gen.nextInt(as.size)).next()

  def sampleFromArray[@spec A: ClassTag](as: Array[A], size: Int)(implicit gen: Generator): Array[A] = {
    val chosen: Array[A] = new Array[A](size)
    if (size < 1) {
      throw new IllegalArgumentException("illegal sample size (%d)" format size)
    } else if (size < as.length) {
      var i = 0
      while (i < as.length) {
        if (i < size) {
          chosen(i) = as(i)
        } else {
          val n: Int = gen.nextInt(i + 1)
          if (n < size) chosen(n) = as(i)
        }
        i += 1
      }

    } else if (size == as.length) {
      System.arraycopy(as, 0, chosen, 0, as.length)
      Shuffling.shuffle(chosen)
    } else {
      throw new IllegalArgumentException("sample size (%d) exceeds input size (%d)" format (size, as.length))
    }
    chosen
  }

  def sampleFromTraversable[@spec A: ClassTag](as: Traversable[A], size: Int)(implicit gen: Generator): Array[A] = {
    val chosen: Array[A] = new Array[A](size)
    var i: Int = 0
    as.foreach { a =>
      if (i < size) {
        chosen(i) = a
      } else {
        val n: Int = gen.nextInt(i + 1)
        if (n < size) chosen(n) = a
      }
      i += 1
    }
    if (i < size) {
      throw new IllegalArgumentException("sample size (%d) exceeds input size (%d)" format (size, i))
    }
    chosen
  }
}

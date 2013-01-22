package spire.math

import spire.algebra.{ AdditiveMonoid }

import scala.reflect.ClassTag
import scala.{ specialized => spec }

trait ArrayEq[@spec(Int,Long,Float,Double) A] extends Eq[Array[A]] {
  implicit def A: Eq[A]
  implicit def classTag: ClassTag[A]

  def eqv(x: Array[A], y: Array[A]): Boolean = {
    if (x.length != y.length) false else {
      var i = 0
      while (i < x.length && i < y.length && A.eqv(x(i), y(i))) {
        i += 1
      }
      i == x.length
    }
  }
}

trait ArrayVectorEq[@spec(Int,Long,Float,Double) A] extends Eq[Array[A]] {
  def scalar: AdditiveMonoid[A]
  def A: Eq[A]

  def eqv(x: Array[A], y: Array[A]): Boolean = {
    var i = 0
    while (i < x.length && i < y.length && A.eqv(x(i), y(i))) {
      i += 1
    }
    while (i < x.length && A.eqv(x(i), scalar.zero)) {
      i += 1
    }
    while (i < y.length && A.eqv(y(i), scalar.zero)) {
      i += 1
    }
    i >= x.length && i >= y.length
  }
}

trait ArrayOrder[@spec(Int,Long,Float,Double) A] extends ArrayEq[A] with Order[Array[A]] {
  implicit def A: Order[A]

  def compare(x: Array[A], y: Array[A]): Int = {
    var i = 0
    var cmp = 0
    while (i < x.length && i < y.length && cmp == 0) {
      cmp = A.compare(x(i), y(i))
      i += 1
    }
    if (cmp == 0) x.length - y.length else cmp
  }
}

trait ArrayVectorOrder[@spec(Int,Long,Float,Double) A] extends ArrayVectorEq[A] with Order[Array[A]] {
  implicit def A: Order[A]

  def compare(x: Array[A], y: Array[A]): Int = {
    var i = 0
    var cmp = 0
    while (i < x.length && i < y.length && cmp == 0) {
      cmp = A.compare(x(i), y(i))
      i += 1
    }
    if (cmp != 0) return cmp
    while (i < x.length) {
      if (!A.eqv(x(i), scalar.zero))
        return 1
      i += 1
    }
    while (i < y.length) {
      if (!A.eqv(y(i), scalar.zero))
        return -1
      i += 1
    }
    0
  }
}

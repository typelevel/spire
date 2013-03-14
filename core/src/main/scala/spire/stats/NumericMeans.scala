package spire.stats

import language.higherKinds

import scala.collection.GenTraversable

import spire.math.Numeric

trait NumericMeans[M[_]] {
  def arithmeticMean[A:Numeric](xs: M[A]): A
  def arithmeticMeanBy[A,B:Numeric](xs: M[A])(f: A ⇒ B): B

  def geometricMean[A:Numeric](xs: M[A]): A
  def geometricMeanBy[A,B:Numeric](xs: M[A])(f: A ⇒ B): B

  def harmonicMean[A:Numeric](xs: M[A]): A
  def harmonicMeanBy[A,B:Numeric](xs: M[A])(f: A ⇒ B): B

  def quadraticMean[A:Numeric](xs: M[A]): A
  def quadraticMeanBy[A,B:Numeric](xs: M[A])(f: A ⇒ B): B
}

trait NumericMeans0 {
  implicit def GenTraversableMeans[CC[X] <: GenTraversable[X]]: NumericMeans[CC] = new GenTraversableNumericMeans[CC] {}
}

object NumericMeans extends NumericMeans0 {
  implicit object ArrayMeans extends ArrayNumericMeans
}

trait ArrayNumericMeans extends NumericMeans[Array] {
  def arithmeticMean[A](xs: Array[A])(implicit num: Numeric[A]): A = {
    val sum = xs.foldLeft(num.zero)(num.plus)
    num.div(sum, num.fromInt(xs.size))
  }

  def arithmeticMeanBy[A,B](xs: Array[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val sum = xs.foldLeft(num.zero)((b,a) ⇒ num.plus(b, f(a)))
    num.div(sum, num.fromInt(xs.size))
  }

  def geometricMean[A](xs: Array[A])(implicit num: Numeric[A]): A = {
    val prod = xs.foldLeft(num.one)(num.times)
    num.fpow(prod, num.reciprocal(num.fromInt(xs.size)))
  }

  def geometricMeanBy[A,B](xs: Array[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val prod = xs.foldLeft(num.one)((b,a) ⇒ num.times(b, f(a)))
    num.fpow(prod, num.reciprocal(num.fromInt(xs.size)))
  }

  def harmonicMean[A](xs: Array[A])(implicit num: Numeric[A]): A = {
    val sum = xs.foldLeft(num.zero)((agg,a) ⇒ num.plus(agg, num.reciprocal(a)))
    num.div(num.fromInt(xs.size), sum)
  }

  def harmonicMeanBy[A,B](xs: Array[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val sum = xs.foldLeft(num.zero)((b,a) ⇒ num.plus(b, num.reciprocal(f(a))))
    num.div(num.fromInt(xs.size), sum)
  }

  def quadraticMean[A](xs: Array[A])(implicit num: Numeric[A]): A = {
    val sum = xs.foldLeft(num.zero)((agg,a) ⇒ num.plus(agg, num.times(a,a)))
    num.sqrt(num.div(sum, num.fromInt(xs.size)))
  }

  def quadraticMeanBy[A,B](xs: Array[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val sum = xs.foldLeft(num.zero)((b,a) ⇒ num.plus(b, num.pow(f(a),2)))
    num.sqrt(num.div(sum, num.fromInt(xs.size)))
  }
}

trait GenTraversableNumericMeans[CC[X] <: GenTraversable[X]] extends NumericMeans[CC] {
  def arithmeticMean[A](xs: CC[A])(implicit num: Numeric[A]): A = {
    val sum = xs.aggregate(num.zero)(num.plus, num.plus)
    num.div(sum, num.fromInt(xs.size))
  }

  def arithmeticMeanBy[A,B](xs: CC[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val sum = xs.aggregate(num.zero)((b,a) ⇒ num.plus(b, f(a)), num.plus)
    num.div(sum, num.fromInt(xs.size))
  }

  def geometricMean[A](xs: CC[A])(implicit num: Numeric[A]): A = {
    val prod = xs.aggregate(num.one)(num.times, num.times)
    num.fpow(prod, num.reciprocal(num.fromInt(xs.size)))
  }

  def geometricMeanBy[A,B](xs: CC[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val prod = xs.aggregate(num.one)((b,a) ⇒ num.times(b, f(a)), num.times)
    num.fpow(prod, num.reciprocal(num.fromInt(xs.size)))
  }

  def harmonicMean[A](xs: CC[A])(implicit num: Numeric[A]): A = {
    val sum = xs.aggregate(num.zero)((agg,a) ⇒ num.plus(agg, num.reciprocal(a)), num.plus)
    num.div(num.fromInt(xs.size), sum)
  }

  def harmonicMeanBy[A,B](xs: CC[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val sum = xs.aggregate(num.zero)((b,a) ⇒ num.plus(b, num.reciprocal(f(a))), num.plus)
    num.div(num.fromInt(xs.size), sum)
  }

  def quadraticMean[A](xs: CC[A])(implicit num: Numeric[A]): A = {
    val sum = xs.aggregate(num.zero)((agg,a) ⇒ num.plus(agg, num.times(a,a)), num.plus)
    num.sqrt(num.div(sum, num.fromInt(xs.size)))
  }

  def quadraticMeanBy[A,B](xs: CC[A])(f: A ⇒ B)(implicit num: Numeric[B]): B = {
    val sum = xs.aggregate(num.zero)((b,a) ⇒ num.plus(b, num.pow(f(a),2)), num.plus)
    num.sqrt(num.div(sum, num.fromInt(xs.size)))
  }
}

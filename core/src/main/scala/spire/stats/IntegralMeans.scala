package spire.stats

import language.higherKinds

import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

import spire.math.Integral

trait IntegralMeans[M[_]] {
  def arithmeticMean[A:Integral](xs: M[A]): A
  def arithmeticMeanBy[A,B:Integral](xs: M[A])(f: A ⇒ B): B
}

trait IntegralMeans0 {
  implicit def GenTraversableMeans[CC[X] <: GenTraversable[X]]: IntegralMeans[CC] = new GenTraversableIntegralMeans[CC] {}
}

object IntegralMeans extends IntegralMeans0 {
  implicit object ArrayMeans extends ArrayIntegralMeans
  implicit object GenTraversableOnceMeans extends GenTraversableOnceIntegralMeans
}

trait ArrayIntegralMeans extends IntegralMeans[Array] {
  def arithmeticMean[A](xs: Array[A])(implicit num: Integral[A]): A = {
    val sum = xs.foldLeft(num.zero)(num.plus)
    num.quot(sum, num.fromInt(xs.size))
  }

  def arithmeticMeanBy[A,B](xs: Array[A])(f: A ⇒ B)(implicit num: Integral[B]): B = {
    val sum = xs.foldLeft(num.zero)((b,a) ⇒ num.plus(b, f(a)))
    num.quot(sum, num.fromInt(xs.size))
  }
}

trait GenTraversableIntegralMeans[CC[X] <: GenTraversable[X]] extends IntegralMeans[CC] {
  def arithmeticMean[A](xs: CC[A])(implicit num: Integral[A]): A = {
    val sum = xs.aggregate(num.zero)(num.plus, num.plus)
    num.quot(sum, num.fromInt(xs.size))
  }

  def arithmeticMeanBy[A,B](xs: CC[A])(f: A ⇒ B)(implicit num: Integral[B]): B = {
    val sum = xs.aggregate(num.zero)((b,a) ⇒ num.plus(b, f(a)), num.plus)
    num.quot(sum, num.fromInt(xs.size))
  }
}

trait GenTraversableOnceIntegralMeans extends IntegralMeans[GenTraversableOnce] {
  def arithmeticMean[A](xs: GenTraversableOnce[A])(implicit num: Integral[A]): A = {
    val seqop = (b: (A,Int), a: A) ⇒ (num.plus(b._1,a), b._2 + 1)
    val combop = (a: (A,Int), b: (A,Int)) ⇒ (num.plus(a._1,b._1), a._2 + b._2)
    val (sum,size) = xs.aggregate((num.zero,0))(seqop, combop)
    num.quot(sum, num.fromInt(size))
  }

  def arithmeticMeanBy[A,B](xs: GenTraversableOnce[A])(f: A ⇒ B)(implicit num: Integral[B]): B = {
    val seqop = (b: (B,Int), a: A) ⇒ (num.plus(b._1,f(a)), b._2 + 1)
    val combop = (a: (B,Int), b: (B,Int)) ⇒ (num.plus(a._1,b._1), a._2 + b._2)
    val (sum,size) = xs.aggregate((num.zero,0))(seqop, combop)
    num.quot(sum, num.fromInt(size))
  }
}

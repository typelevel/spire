package spire.syntax.std

import spire.algebra._
import spire.math._
import spire.macrosk._

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.{specialized => spec}

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

final class LiteralIntOps(val lhs: Int) extends AnyVal {
  def /~(rhs: Int) = lhs / rhs
  def /%(rhs: Int) = (lhs / rhs, lhs % rhs)
  def pow(rhs: Int) = Math.pow(lhs, rhs).toInt
  def **(rhs: Int) = Math.pow(lhs, rhs).toInt
  def !(): BigInt = spire.math.fact(lhs)
}

final class LiteralLongOps(val lhs: Long) extends AnyVal {
  def /~(rhs: Long) = lhs / rhs
  def /%(rhs: Long) = (lhs / rhs, lhs % rhs)
  def pow(rhs: Long) = spire.math.pow(lhs, rhs)
  def **(rhs: Long) = spire.math.pow(lhs, rhs)
  def !(): BigInt = spire.math.fact(lhs)
}

final class LiteralDoubleOps(val lhs: Double) extends AnyVal {
  def pow(rhs: Double) = spire.math.pow(lhs, rhs)
  def **(rhs: Double) = spire.math.pow(lhs, rhs)
}

class LiteralBigIntOps(val lhs: BigInt) extends AnyVal {
  def /~(rhs: BigInt) = lhs / rhs
  def pow(rhs: BigInt) = spire.math.pow(lhs, rhs)
  def **(rhs: BigInt) = spire.math.pow(lhs, rhs)

  def +(rhs: SafeLong) = SafeLong(lhs) + rhs
  def *(rhs: SafeLong) = SafeLong(lhs) * rhs
  def -(rhs: SafeLong) = SafeLong(lhs) - rhs
  def /(rhs: SafeLong) = SafeLong(lhs) / rhs
  def /~(rhs: SafeLong) = SafeLong(lhs) /~ rhs
  def %(rhs: SafeLong) = SafeLong(lhs) % rhs
  def /%(rhs: SafeLong) = SafeLong(lhs) /% rhs

  def +(rhs: Natural) = Natural(lhs) + rhs
  def *(rhs: Natural) = Natural(lhs) * rhs
  def -(rhs: Natural) = Natural(lhs) - rhs
  def /(rhs: Natural) = Natural(lhs) / rhs
  def /~(rhs: Natural) = Natural(lhs) /~ rhs
  def %(rhs: Natural) = Natural(lhs) % rhs
  def /%(rhs: Natural) = Natural(lhs) /% rhs

  def +(rhs: ULong) = lhs + rhs.toBigInt
  def *(rhs: ULong) = lhs * rhs.toBigInt
  def -(rhs: ULong) = lhs - rhs.toBigInt
  def /(rhs: ULong) = lhs / rhs.toBigInt
  def /~(rhs: ULong) = lhs / rhs.toBigInt
  def %(rhs: ULong) = lhs % rhs.toBigInt
  def /%(rhs: ULong) = lhs /% rhs.toBigInt

  def +(rhs: Number) = Number(lhs) + rhs
  def *(rhs: Number) = Number(lhs) * rhs
  def -(rhs: Number) = Number(lhs) - rhs
  def /(rhs: Number) = Number(lhs) / rhs
  def /~(rhs: Number) = Number(lhs) / rhs
  def %(rhs: Number) = Number(lhs) % rhs
  def /%(rhs: Number) = Number(lhs) /% rhs
}

final class ArrayOps[@spec A](arr: Array[A]) {
  def qsum(implicit ev: AdditiveMonoid[A]) = {
    @tailrec
    def f(i: Int, n: Int, t: A): A = if (i < n) f(i + 1, n, ev.plus(t, arr(i))) else t
    f(0, arr.length, ev.zero)
  }

  def qproduct(implicit ev: MultiplicativeMonoid[A]) = {
    @tailrec
    def f(i: Int, n: Int, t: A): A = if (i < n) f(i + 1, n, ev.times(t, arr(i))) else t
    f(0, arr.length, ev.one)
  }

  def qnorm(p: Int)(implicit ev: Field[A], s: Signed[A], nr: NRoot[A]) = {
    @tailrec
    def f(i: Int, n: Int, t: A): A =
      if (i < n) f(i + 1, n, ev.plus(t, ev.pow(s.abs(arr(i)), p))) else t
    nr.nroot(f(0, arr.length, ev.one), p)
  }

  def qmin(implicit ev: Order[A]) = {
    if (arr.length == 0) sys.error("empty array")
    @tailrec
    def f(i: Int, n: Int, t: A): A = if (i < n) f(i + 1, n, ev.min(t, arr(i))) else t
    f(1, arr.length, arr(0))
  }

  def qmax(implicit ev: Order[A]) = {
    if (arr.length == 0) sys.error("empty array")
    @tailrec
    def f(i: Int, n: Int, t: A): A = if (i < n) f(i + 1, n, ev.max(t, arr(i))) else t
    f(1, arr.length, arr(0))
  }

  def qmean(implicit ev: Field[A]): A = {
    if (arr.length == 0) sys.error("empty array")
    var sum = ev.zero
    @tailrec
    def f(i: Int, j: Int, n: Int): A = if (i < n) {
      val t = ev.div(ev.times(sum, ev.fromInt(i)), ev.fromInt(j))
      val z = ev.div(arr(i), ev.fromInt(j))
      sum = ev.plus(t, z)
      f(i + 1, j + 1, n)
    } else {
      sum
    }
    f(0, 1, arr.length)
  }

  import spire.math.{Sorting, Selection, Searching}

  def qsearch(a: A)(implicit ev: Order[A]): Int = {
    Searching.search(arr, a)
  }

  def qsort(implicit ev: Order[A], ct: ClassTag[A]) {
    Sorting.sort(arr)
  }

  def qsortBy[@spec B](f: A => B)(implicit ev: Order[B], ct: ClassTag[A]) {
    implicit val ord: Order[A] = ev.on(f)
    Sorting.sort(arr)
  }

  def qsortWith(f: (A, A) => Int)(implicit ct: ClassTag[A]) {
    implicit val ord: Order[A] = Order.from(f)
    Sorting.sort(arr)
  }

  def qsorted(implicit ev: Order[A], ct: ClassTag[A]): Array[A] = {
    val arr2 = arr.clone
    Sorting.sort(arr2)
    arr2
  }

  def qsortedBy[@spec B](f: A => B)(implicit ev: Order[B], ct: ClassTag[A]): Array[A] = {
    implicit val ord: Order[A] = ev.on(f)
    val arr2 = arr.clone
    Sorting.sort(arr2)
    arr2
  }

  def qsortedWith(f: (A, A) => Int)(implicit ct: ClassTag[A]): Array[A] = {
    implicit val ord: Order[A] = Order.from(f)
    val arr2 = arr.clone
    Sorting.sort(arr2)
    arr2
  }

  def qselect(k: Int)(implicit ev: Order[A], ct: ClassTag[A]) {
    Selection.select(arr, k)
  }

  def qselected(k: Int)(implicit ev: Order[A], ct: ClassTag[A]): Array[A] = {
    val arr2 = arr.clone
    Selection.select(arr2, k)
    arr2
  }

  import spire.random.mutable.Generator

  def qshuffle()(implicit gen: Generator): Unit = gen.shuffle(arr)

  def qshuffled(implicit gen: Generator): Array[A] = {
    val arr2 = arr.clone
    gen.shuffle(arr2)
    arr2
  }

  def qsampled(n: Int)(implicit gen: Generator, ct: ClassTag[A]): Array[A] =
    gen.sampleFromArray(arr, n)
}

final class IndexedSeqOps[@spec A, CC[A] <: IndexedSeq[A]](as: CC[A]) {
  def qsearch(a: A)(implicit ev: Order[A]): Int =
    Searching.search(as, a)
}

final class SeqOps[@spec A, CC[A] <: Iterable[A]](as: CC[A]) {
  def qsum(implicit ev: AdditiveMonoid[A]) = {
    var sum = ev.zero
    val f: A => Unit = (a: A) => sum = ev.plus(sum, a)
    as.foreach(f)
    sum
  }

  def qproduct(implicit ev: MultiplicativeMonoid[A]) = {
    var prod = ev.one
    as.foreach(a => prod = ev.times(prod, a))
    prod
  }

  def qnorm(p: Int)(implicit ev: Field[A], s: Signed[A], nr: NRoot[A]) = {
    var t = ev.one
    as.foreach(a => t = ev.plus(t, ev.pow(s.abs(a), p)))
    nr.nroot(t, p)
  }

  def qmin(implicit ev: Order[A]) = {
    if (as.isEmpty) sys.error("empty seq")
    var min = as.head
    as.foreach(a => min = ev.min(min, a))
    min
  }

  def qmax(implicit ev: Order[A]) = {
    if (as.isEmpty) sys.error("empty seq")
    var max = as.head
    as.foreach(a => max = ev.max(max, a))
    max
  }

  def qmean(implicit ev: Field[A]): A = {
    if (as.isEmpty) sys.error("empty seq")
    var mean = ev.zero
    var i = 0
    var j = 1
    as.foreach { a =>
      val t = ev.div(ev.times(mean, ev.fromInt(i)), ev.fromInt(j))
      val z = ev.div(a, ev.fromInt(j))
      mean = ev.plus(t, z)
      i += 1
      j += 1
    }
    mean
  }

  import spire.math.{Sorting, Selection}

  protected[this] def toSizeAndArray(implicit ct: ClassTag[A]) = {
    val len = as.size
    val arr = new Array[A](len)
    var i = 0
    as.foreach { a =>
      arr(i) = a
      i += 1
    }
    (len, arr)
  }

  protected[this] def fromSizeAndArray(len: Int, arr: Array[A])(implicit cbf: CanBuildFrom[CC[A], A, CC[A]]) = {
    val b = cbf(as)
    b.sizeHint(len)
    var i = 0
    while (i < len) {
      b += arr(i)
      i += 1
    }
    b.result
  }

  def qsorted(implicit ev: Order[A], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val (len, arr) = toSizeAndArray
    Sorting.sort(arr)
    fromSizeAndArray(len, arr)
  }
  
  def qsortedBy[@spec B](f: A => B)(implicit ev: Order[B], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val (len, arr) = toSizeAndArray
    implicit val ord: Order[A] = ev.on(f)
    Sorting.sort(arr)
    fromSizeAndArray(len, arr)
  }
  
  def qsortedWith(f: (A, A) => Int)(implicit ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val (len, arr) = toSizeAndArray
    implicit val ord: Order[A] = Order.from(f)
    Sorting.sort(arr)
    fromSizeAndArray(len, arr)
  }
  
  def qselected(k: Int)(implicit ev: Order[A], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val (len, arr) = toSizeAndArray
    Selection.select(arr, k)
    fromSizeAndArray(len, arr)
  }

  import spire.random.mutable.Generator

  def qshuffled(implicit gen: Generator, ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val (len, arr) = toSizeAndArray
    gen.shuffle(arr)
    fromSizeAndArray(len, arr)
  }

  def qsampled(n: Int)(implicit gen: Generator, ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val sampled = gen.sampleFromTraversable(as, n)
    fromSizeAndArray(n, sampled)
  }

  def qchoose(implicit gen: Generator): A = gen.chooseFromIterable(as)
}

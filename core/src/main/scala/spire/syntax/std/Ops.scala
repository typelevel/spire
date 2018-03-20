package spire
package syntax
package std

import scala.collection.generic.CanBuildFrom
import spire.algebra.{AdditiveMonoid, Field, Monoid, MultiplicativeMonoid, NRoot, Order, PartialOrder, Signed}
import spire.math.{Natural, Number, QuickSort, SafeLong, Searching, ULong}
import spire.syntax.cfor._
import spire.syntax.monoid._
import spire.syntax.field._
import spire.syntax.nroot._
import spire.syntax.signed._

final class LiteralIntOps(val lhs: Int) extends AnyVal {
  def /~(rhs: Int): Int = lhs / rhs
  def /%(rhs: Int): (Int, Int) = (lhs / rhs, lhs % rhs)
  def pow(rhs: Int): Int = Math.pow(lhs, rhs).toInt
  def **(rhs: Int): Int = Math.pow(lhs, rhs).toInt
  def !(): BigInt = spire.math.fact(lhs)
  def choose(rhs: Int): BigInt = spire.math.choose(lhs, rhs)
}

final class LiteralLongOps(val lhs: Long) extends AnyVal {
  def /~(rhs: Long): Long = lhs / rhs
  def /%(rhs: Long): (Long, Long) = (lhs / rhs, lhs % rhs)
  def pow(rhs: Long): Long = spire.math.pow(lhs, rhs)
  def **(rhs: Long): Long = spire.math.pow(lhs, rhs)
  def !(): BigInt = spire.math.fact(lhs)
  def choose(rhs: Long): BigInt = spire.math.choose(lhs, rhs)
}

final class LiteralDoubleOps(val lhs: Double) extends AnyVal {
  def pow(rhs: Double): Double = spire.math.pow(lhs, rhs)
  def **(rhs: Double): Double = spire.math.pow(lhs, rhs)
}

class LiteralBigIntOps(val lhs: BigInt) extends AnyVal {
  def /~(rhs: BigInt): BigInt = lhs / rhs
  def pow(rhs: BigInt): BigInt = spire.math.pow(lhs, rhs)
  def **(rhs: BigInt): BigInt = spire.math.pow(lhs, rhs)

  def +(rhs: SafeLong): SafeLong = SafeLong(lhs) + rhs
  def *(rhs: SafeLong): SafeLong = SafeLong(lhs) * rhs
  def -(rhs: SafeLong): SafeLong = SafeLong(lhs) - rhs
  def /(rhs: SafeLong): SafeLong = SafeLong(lhs) / rhs
  def /~(rhs: SafeLong): SafeLong = SafeLong(lhs) /~ rhs
  def %(rhs: SafeLong): SafeLong = SafeLong(lhs) % rhs
  def /%(rhs: SafeLong): (SafeLong, SafeLong) = SafeLong(lhs) /% rhs

  def +(rhs: Natural): BigInt = lhs + rhs.toBigInt
  def *(rhs: Natural): BigInt = lhs * rhs.toBigInt
  def -(rhs: Natural): BigInt = lhs - rhs.toBigInt
  def /(rhs: Natural): BigInt = lhs / rhs.toBigInt
  def /~(rhs: Natural): BigInt = lhs / rhs.toBigInt
  def %(rhs: Natural): BigInt = lhs % rhs.toBigInt
  def /%(rhs: Natural): (BigInt, BigInt) = lhs /% rhs.toBigInt

  def +(rhs: ULong): BigInt = lhs + rhs.toBigInt
  def *(rhs: ULong): BigInt = lhs * rhs.toBigInt
  def -(rhs: ULong): BigInt = lhs - rhs.toBigInt
  def /(rhs: ULong): BigInt = lhs / rhs.toBigInt
  def /~(rhs: ULong): BigInt = lhs / rhs.toBigInt
  def %(rhs: ULong): BigInt = lhs % rhs.toBigInt
  def /%(rhs: ULong): (BigInt, BigInt) = lhs /% rhs.toBigInt

  def +(rhs: Number): Number = Number(lhs) + rhs
  def *(rhs: Number): Number = Number(lhs) * rhs
  def -(rhs: Number): Number = Number(lhs) - rhs
  def /(rhs: Number): Number = Number(lhs) / rhs
  def /~(rhs: Number): Number = Number(lhs) / rhs
  def %(rhs: Number): Number = Number(lhs) emod rhs
  def /%(rhs: Number): (Number, Number) = Number(lhs) equotmod rhs
}

final class ArrayOps[@sp A](arr: Array[A]) {
  def qsum(implicit ev: AdditiveMonoid[A]): A = {
    var result = ev.zero
    cfor(0)(_ < arr.length, _ + 1) { i => result += arr(i) }
    result
  }

  def qproduct(implicit ev: MultiplicativeMonoid[A]): A = {
    var result = ev.one
    cfor(0)(_ < arr.length, _ + 1) { i => result *= arr(i) }
    result
  }

  def qcombine(implicit ev: Monoid[A]): A = {
    var result = ev.empty
    cfor(0)(_ < arr.length, _ + 1) { i => result |+|= arr(i) }
    result
  }

  def qnorm(p: Int)(implicit ev: Field[A], s: Signed[A], nr: NRoot[A]): A = {
    var result = ev.one
    cfor(0)(_ < arr.length, _ + 1) { i => result += arr(i).abs.pow(p) }
    result.nroot(p)
  }

  def qnormWith[@sp(Double) R](p: Int)(f: A => R)(implicit ev: Field[R], s: Signed[R], nr: NRoot[R]): R = {
    var result: R = ev.one
    cfor(0)(_ < arr.length, _ + 1) { i => result += f(arr(i)).abs.pow(p) }
    result.nroot(p)
  }

  def qmin(implicit ev: Order[A]): A = {
    if (arr.length == 0) throw new UnsupportedOperationException("empty array")
    var result = arr(0)
    cfor(1)(_ < arr.length, _ + 1) { i =>
      result = result min arr(i)
    }
    result
  }

  def qmax(implicit ev: Order[A]): A = {
    if (arr.length == 0) throw new UnsupportedOperationException("empty array")
    var result = arr(0)
    cfor(1)(_ < arr.length, _ + 1) { i =>
      result = result max arr(i)
    }
    result
  }

  def qmean(implicit ev: Field[A]): A = {
    if (arr.length == 0) throw new UnsupportedOperationException("empty array")
    var result = ev.zero
    cfor(0)(_ < arr.length, _ + 1) { i =>
      result = (result * i / (i + 1)) + (arr(i) / (i + 1))
    }
    result
  }

  def qmeanWith[@sp(Double) R](f: A => R)(implicit ev: Field[R]): R = {
    if (arr.length == 0) throw new UnsupportedOperationException("empty array")
    var result: R = ev.zero
    cfor(0)(_ < arr.length, _ + 1) { i =>
      result = (result * i / (i + 1)) + (f(arr(i)) / (i + 1))
    }
    result
  }

  import spire.math.{Sorting, Selection, Searching}

  def qsearch(a: A)(implicit ev: Order[A]): Int = {
    Searching.search(arr, a)
  }

  def qsort(implicit ev: Order[A], ct: ClassTag[A]): Unit = {
    Sorting.sort(arr)
  }

  def qsortBy[@sp B](f: A => B)(implicit ev: Order[B], ct: ClassTag[A]): Unit = {
    implicit val ord: Order[A] = Order.by(f)
    Sorting.sort(arr)
  }

  def qsortWith(f: (A, A) => Int)(implicit ct: ClassTag[A]): Unit = {
    implicit val ord: Order[A] = Order.from(f)
    Sorting.sort(arr)
  }

  def qsorted(implicit ev: Order[A], ct: ClassTag[A]): Array[A] = {
    val arr2 = arr.clone
    Sorting.sort(arr2)
    arr2
  }

  def qsortedBy[@sp B](f: A => B)(implicit ev: Order[B], ct: ClassTag[A]): Array[A] = {
    implicit val ord: Order[A] = Order.by(f)
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

  def qselect(k: Int)(implicit ev: Order[A], ct: ClassTag[A]): Unit = {
    Selection.select(arr, k)
  }

  def qselected(k: Int)(implicit ev: Order[A], ct: ClassTag[A]): Array[A] = {
    val arr2 = arr.clone
    Selection.select(arr2, k)
    arr2
  }

  import spire.random.Generator

  def qshuffle()(implicit gen: Generator): Unit = gen.shuffle(arr)

  def qshuffled(implicit gen: Generator): Array[A] = {
    val arr2 = arr.clone
    gen.shuffle(arr2)
    arr2
  }

  def qsampled(n: Int)(implicit gen: Generator, ct: ClassTag[A]): Array[A] =
    gen.sampleFromArray(arr, n)
}

final class IndexedSeqOps[@sp A, CC[A] <: IndexedSeq[A]](as: CC[A]) {
  def qsearch(a: A)(implicit ev: Order[A]): Int =
    Searching.search(as, a)
}

final class SeqOps[@sp A, CC[A] <: Iterable[A]](as: CC[A]) { //fixme
  def qsum(implicit ev: AdditiveMonoid[A]): A =
    as.aggregate(ev.zero)(ev.plus, ev.plus)

  def qproduct(implicit ev: MultiplicativeMonoid[A]): A =
    as.aggregate(ev.one)(ev.times, ev.times)

  def qcombine(implicit ev: Monoid[A]): A =
    as.aggregate(ev.empty)(ev.combine, ev.combine)

  def qnorm(p: Int)(implicit ev: Field[A], s: Signed[A], nr: NRoot[A]): A =
    as.aggregate(ev.one)(_ + _.abs.pow(p), _ + _).nroot(p)

  def qnormWith[R](p: Int)(f: A => R)(implicit ev: Field[R], s: Signed[R], nr: NRoot[R]): R =
    as.aggregate(ev.one)((t, a) => t + f(a).abs.pow(p), _ + _).nroot(p)

  /** Computes the minimal elements of a partially ordered set.
   * If the poset contains multiple copies of a minimal element, the function
   * will only return a single copy of it.
   */
  def pmin(implicit ev: PartialOrder[A]): Seq[A] =
    Searching.minimalElements(as)(ev)

  /** Computes the maximal elements of a partially ordered set.
   * If the posset contains multiple copies of a maximal element, the function
   * will only return a single copy of it.
   */
  def pmax(implicit ev: PartialOrder[A]): Seq[A] =
    Searching.minimalElements(as)(PartialOrder.reverse(ev))

  def qmin(implicit ev: Order[A]): A = {
    if (as.isEmpty) throw new UnsupportedOperationException("empty seq")
    as.aggregate(as.head)(ev.min, ev.min)
  }

  def qmax(implicit ev: Order[A]): A = {
    if (as.isEmpty) throw new UnsupportedOperationException("empty seq")
    as.aggregate(as.head)(ev.max, ev.max)
  }

  def qmean(implicit ev: Field[A]): A = {
    if (as.isEmpty) throw new UnsupportedOperationException("empty seq")
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

  def qmeanWith[R](f: A => R)(implicit ev: Field[R]): R = {
    if (as.isEmpty) throw new UnsupportedOperationException("empty seq")
    var mean = ev.zero
    var i = 0
    var j = 1
    as.foreach { a =>
      val t = ev.div(ev.times(mean, ev.fromInt(i)), ev.fromInt(j))
      val z = ev.div(f(a), ev.fromInt(j))
      mean = ev.plus(t, z)
      i += 1
      j += 1
    }
    mean
  }

  import spire.math.{Sorting, Selection}

  protected[this] def fromArray(arr: Array[A])(implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val b = cbf(as)
    b.sizeHint(arr.length)
    cfor(0)(_ < arr.length, _ + 1) { i => b += arr(i) }
    b.result
  }

  protected[this] def fromSizeAndArray(size: Int, arr: Array[A])(implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val b = cbf(as)
    b.sizeHint(size)
    cfor(0)(_ < size, _ + 1) { i => b += arr(i) }
    b.result
  }

  def qsorted(implicit ev: Order[A], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val arr = as.toArray
    Sorting.sort(arr)
    fromArray(arr)
  }

  def qsortedBy[@sp B](f: A => B)(implicit ev: Order[B], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    implicit val ord: Order[A] = Order.by(f)
    val arr = as.toArray
    Sorting.sort(arr)
    fromArray(arr)
  }

  def qsortedWith(f: (A, A) => Int)(implicit ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    implicit val ord: Order[A] = Order.from(f)
    val arr = as.toArray
    Sorting.sort(arr)
    fromArray(arr)
  }

  def qselected(k: Int)(implicit ev: Order[A], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val arr = as.toArray
    Selection.select(arr, k)
    fromArray(arr)
  }

  def qselectk(k: Int)(implicit ev: Order[A], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val arr = as.toArray
    if (arr.length <= k) {
      fromArray(arr)
    } else {
      Selection.select(arr, k)
      fromSizeAndArray(k, arr)
    }
  }

  def qtopk(k: Int)(implicit ev: Order[A], ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val arr = as.toArray
    if (arr.length <= k) {
      Sorting.sort(arr)
      fromArray(arr)
    } else {
      Selection.select(arr, k)
      QuickSort.qsort(arr, 0, k)
      fromSizeAndArray(k, arr)
    }
  }

  import spire.random.Generator

  def qshuffled(implicit gen: Generator, ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    val arr = as.toArray
    gen.shuffle(arr)
    fromArray(arr)
  }

  def qsampled(n: Int)(implicit gen: Generator, ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] =
    fromArray(gen.sampleFromTraversable(as, n))

  def qchoose(implicit gen: Generator): A =
    gen.chooseFromIterable(as)
}

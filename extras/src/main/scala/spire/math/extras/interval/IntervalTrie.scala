package spire
package math.extras.interval

import spire.algebra.{Bool, Eq, Order}
import spire.math._
import spire.math.interval._

import scala.annotation.tailrec

sealed abstract class IntervalTrie[T] extends IntervalSet[T, IntervalTrie[T]]

object IntervalTrie {

  implicit def algebra[T: Element]: Bool[IntervalTrie[T]] with Eq[IntervalTrie[T]] = new Bool[IntervalTrie[T]]
    with Eq[IntervalTrie[T]] {

    def eqv(x: IntervalTrie[T], y: IntervalTrie[T]): Boolean = x == y

    def zero: IntervalTrie[T] = IntervalTrie.empty[T]

    def one: IntervalTrie[T] = IntervalTrie.all[T]

    def complement(a: IntervalTrie[T]): IntervalTrie[T] = ~a

    def or(a: IntervalTrie[T], b: IntervalTrie[T]): IntervalTrie[T] = a | b

    def and(a: IntervalTrie[T], b: IntervalTrie[T]): IntervalTrie[T] = a & b

    override def xor(a: IntervalTrie[T], b: IntervalTrie[T]): IntervalTrie[T] = a ^ b
  }

  trait Element[@sp(Float, Int, Long, Double) T] {

    implicit def order: Order[T]

    def toLong(value: T): Long

    def fromLong(key: Long): T
  }

  implicit object ByteElement extends Element[Byte] {

    def order: Order[Byte] = spire.std.byte.ByteAlgebra

    def toLong(value: Byte): Long = value

    def fromLong(key: Long): Byte = key.toByte
  }

  implicit object ShortElement extends Element[Short] {

    def order: Order[Short] = spire.std.short.ShortAlgebra

    def toLong(value: Short): Long = value

    def fromLong(key: Long): Short = key.toShort
  }

  implicit object IntElement extends Element[Int] {

    def order: Order[Int] = spire.std.int.IntAlgebra

    def toLong(value: Int): Long = value

    def fromLong(key: Long): Int = key.toInt
  }

  implicit object LongElement extends Element[Long] {

    def order: Order[Long] = spire.std.long.LongAlgebra

    def toLong(value: Long): Long = value

    def fromLong(key: Long): Long = key
  }

  implicit object FloatElement extends Element[Float] {

    def order: Order[Float] = spire.std.float.FloatAlgebra

    def toLong(value: Float): Long = {
      if (java.lang.Float.isNaN(value))
        throw new IllegalArgumentException("NaN")
      // sign and magnitude signed integer
      val signAndMagnitude = java.lang.Float.floatToIntBits(value)
      // two's complement signed integer: if the sign bit is set, negate everything except the sign bit
      val twosComplement = if (signAndMagnitude >= 0) signAndMagnitude else (-signAndMagnitude | (1L << 63))
      twosComplement
    }

    def fromLong(twosComplement: Long): Float = {
      // sign and magnitude signed integer: if the sign bit is set, negate everything except the sign bit
      val signAndMagnitude = if (twosComplement >= 0) twosComplement else (-twosComplement | (1L << 63))
      // double from sign and magnitude signed integer
      java.lang.Float.intBitsToFloat(signAndMagnitude.toInt)
    }
  }

  implicit object CharElement extends Element[Char] {

    def order: Order[Char] = spire.std.char.CharAlgebra

    def toLong(value: Char): Long = value.toLong

    def fromLong(key: Long): Char = key.toChar
  }

  implicit object DoubleElement extends Element[Double] {

    def order: Order[Double] = spire.std.double.DoubleAlgebra

    def toLong(value: Double): Long = {
      if (java.lang.Double.isNaN(value))
        throw new IllegalArgumentException("NaN")
      // sign and magnitude signed integer
      val signAndMagnitude = java.lang.Double.doubleToLongBits(value)
      // two's complement signed integer: if the sign bit is set, negate everything except the sign bit
      val twosComplement = if (signAndMagnitude >= 0) signAndMagnitude else (-signAndMagnitude | (1L << 63))
      twosComplement
    }

    def fromLong(twosComplement: Long): Double = {
      // sign and magnitude signed integer: if the sign bit is set, negate everything except the sign bit
      val signAndMagnitude = if (twosComplement >= 0) twosComplement else (-twosComplement | (1L << 63))
      // double from sign and magnitude signed integer
      java.lang.Double.longBitsToDouble(signAndMagnitude)
    }
  }

  implicit object UByteElement extends Element[UByte] {

    def order: Order[UByte] = spire.math.UByte.UByteAlgebra

    def toLong(value: UByte): Long = value.toLong

    def fromLong(key: Long): UByte = UByte(key.toByte)
  }

  implicit object UShortElement extends Element[UShort] {

    def order: Order[UShort] = spire.math.UShort.UShortAlgebra

    def toLong(value: UShort): Long = value.toLong

    def fromLong(key: Long): UShort = UShort(key.toShort)
  }

  implicit object UIntElement extends Element[UInt] {

    def order: Order[UInt] = spire.math.UInt.UIntAlgebra

    def toLong(value: UInt): Long = value.toLong

    def fromLong(key: Long): UInt = UInt(key.toInt)
  }

  implicit object ULongElement extends Element[ULong] {

    def order: Order[ULong] = spire.math.ULong.ULongAlgebra

    def toLong(value: ULong): Long = value.toLong + Long.MinValue

    def fromLong(key: Long): ULong = ULong(key - Long.MinValue)
  }

  import Tree._

  implicit private def tIsLong[T](value: T)(implicit tl: Element[T]) = tl.toLong(value)

  private[interval] def fromKind[T: Element](value: T, kind: Int) = {
    val bound = kind match {
      case 0 => Below(value)
      case 1 => Above(value)
      case 2 => Both(value)
    }
    IntervalTrie[T](false, bound)
  }

  def constant[T: Element](value: Boolean): IntervalTrie[T] = IntervalTrie[T](value, null)

  def empty[T: Element]: IntervalTrie[T] = constant[T](false)

  def point[T: Element](value: T): IntervalTrie[T] = IntervalTrie[T](false, Tree.Leaf(toPrefix(value), true, false))

  def atOrAbove[T: Element](value: T): IntervalTrie[T] = IntervalTrie[T](false, Tree.Leaf(toPrefix(value), true, true))

  def above[T: Element](value: T): IntervalTrie[T] = IntervalTrie[T](false, Tree.Leaf(toPrefix(value), false, true))

  def all[T: Element]: IntervalTrie[T] = constant[T](true)

  def hole[T: Element](value: T): IntervalTrie[T] = IntervalTrie[T](true, Tree.Leaf(toPrefix(value), true, false))

  def below[T: Element](value: T): IntervalTrie[T] = IntervalTrie[T](true, Tree.Leaf(toPrefix(value), true, true))

  def atOrBelow[T: Element](value: T): IntervalTrie[T] = IntervalTrie[T](true, Tree.Leaf(toPrefix(value), false, true))

  def apply[T: Element](interval: Interval[T]): IntervalTrie[T] = interval.fold {
    case (Closed(a), Closed(b)) if a == b => point(a)
    case (Unbound(), Open(x))             => below(x)
    case (Unbound(), Closed(x))           => atOrBelow(x)
    case (Open(x), Unbound())             => above(x)
    case (Closed(x), Unbound())           => atOrAbove(x)
    case (Closed(a), Closed(b))           => fromTo(Below(a), Above(b))
    case (Closed(a), Open(b))             => fromTo(Below(a), Below(b))
    case (Open(a), Closed(b))             => fromTo(Above(a), Above(b))
    case (Open(a), Open(b))               => fromTo(Above(a), Below(b))
    case (Unbound(), Unbound())           => all[T]
    case (EmptyBound(), EmptyBound())     => empty[T]
    case _                                => sys.error("no")
  }

  private object Below {

    def apply[T: Element](value: T) = Leaf(toPrefix(value), true, true)

    def unapply(l: Leaf) = if (l.at && l.sign) Some(l.key) else None
  }

  private object Above {

    def apply[T: Element](value: T) = Leaf(toPrefix(value), false, true)

    def unapply(l: Leaf) = if (!l.at && l.sign) Some(l.key) else None
  }

  private object Both {

    def apply[T: Element](value: T) = Leaf(toPrefix(value), true, false)

    def unapply(l: Leaf) = if (l.at && !l.sign) Some(l.key) else None
  }

  private def fromTo[T: Element](a: Leaf, b: Leaf): IntervalTrie[T] = {
    IntervalTrie[T](false, concat(a, b))
  }

  def apply(text: String): IntervalTrie[Long] = {
    val la = spire.std.long.LongAlgebra
    def rationalToLong(r: Rational): Long = {
      if (r > Long.MaxValue || r < Long.MinValue)
        throw new NumberFormatException("Integer number too large")
      else
        r.toLong
    }
    def intervalToIntervalSet(i: Interval[Long]): IntervalTrie[Long] = apply(i)
    val intervals = text.split(';').map(Interval.apply).map(_.mapBounds(rationalToLong)(la))
    val simpleSets = intervals.map(intervalToIntervalSet)
    simpleSets.foldLeft(empty[Long])(_ | _)
  }

  final private def foreachInterval[T: Element, U](a0: Boolean, a: Tree)(f: Interval[T] => U): Unit = {
    val x = implicitly[Element[T]]
    import x._
    def op(b0: Bound[T], a0: Boolean, a: Tree): Bound[T] = a match {
      case Below(a) =>
        if (a0)
          f(Interval.fromBounds(b0, Open(fromLong(a))))
        Closed(fromLong(a))
      case Above(a) =>
        if (a0)
          f(Interval.fromBounds(b0, Closed(fromLong(a))))
        Open(fromLong(a))
      case Both(a) =>
        if (a0)
          f(Interval.fromBounds(b0, Open(fromLong(a))))
        else
          f(Interval.point(fromLong(a)))
        Open(fromLong(a))
      case a: Branch =>
        val am = a0 ^ a.left.sign
        val bm = op(b0, a0, a.left)
        val b1 = op(bm, am, a.right)
        b1
      case _ =>
        Unbound()
    }
    val last = op(Unbound(), a0, a)
    if (a0 ^ ((a ne null) && a.sign))
      f(Interval.fromBounds(last, Unbound()))
  }

  abstract private class TreeIterator[T](a: Tree) extends Iterator[T] {

    var index = 0
    var buffer = new Array[Tree](65)

    def pop() = {
      index -= 1
      buffer(index)
    }

    def push(x: Tree): Unit = {
      buffer(index) = x
      index += 1
    }

    if (a ne null)
      push(a)

    def hasNextLeaf = index != 0

    final def nextLeaf(): Leaf = pop() match {
      case b: Branch =>
        push(b.right)
        push(b.left)
        nextLeaf()
      case l: Leaf => l
      // $COVERAGE-OFF$
      case _ => unreachable
      // $COVERAGE-ON$
    }
  }

  final private class EdgeIterator[T: Element](tree: Tree) extends TreeIterator[T](tree) {
    private val element = implicitly[Element[T]]

    def hasNext = hasNextLeaf

    override def next() = element.fromLong(nextLeaf().key)
  }

  final private class IntervalIterator[T: Element](e: IntervalTrieImpl[T]) extends TreeIterator[Interval[T]](e.tree) {

    private[this] val element = implicitly[Element[T]]

    private[this] var lower: Bound[T] = if (e.belowAll) Unbound() else null

    private[this] def nextInterval(): Interval[T] = {
      import element.{fromLong, order}
      var result: Interval[T] = null
      if (hasNextLeaf) {
        val leaf = nextLeaf()
        if (lower eq null) leaf match {
          case Both(x) =>
            result = Interval.point(fromLong(x))
            lower = null
          case Below(x) =>
            result = null
            lower = Closed(fromLong(x))
          case Above(x) =>
            result = null
            lower = Open(fromLong(x))

          // $COVERAGE-OFF$
          case _ => unreachable
          // $COVERAGE-ON$
        }
        else
          leaf match {
            case Both(x) =>
              val upper = Open(fromLong(x))
              result = Interval.fromBounds[T](lower, upper)
              lower = upper
            case Below(x) =>
              val upper = Open(fromLong(x))
              result = Interval.fromBounds[T](lower, upper)
              lower = null
            case Above(x) =>
              val upper = Closed(fromLong(x))
              result = Interval.fromBounds[T](lower, upper)
              lower = null
            // $COVERAGE-OFF$
            case _ => unreachable
            // $COVERAGE-ON$
          }
      } else if (lower ne null) {
        result = Interval.fromBounds(lower, Unbound())
        lower = null
      } else {
        Iterator.empty.next()
      }
      result
    }

    def hasNext: Boolean = hasNextLeaf || (lower ne null)

    @tailrec
    override def next(): Interval[T] = {
      val result = nextInterval()
      if (result ne null)
        result
      else
        next()
    }
  }

  private def apply[T: Element](below: Boolean, tree: Tree): IntervalTrie[T] =
    IntervalTrieImpl(below, tree)

  final private case class IntervalTrieImpl[T](belowAll: Boolean, tree: Tree)(implicit ise: Element[T])
      extends IntervalTrie[T] { lhs =>

    import Tree._
    import ise.order

    def aboveAll: Boolean = if (tree eq null) belowAll else belowAll ^ tree.sign

    def isEmpty = !belowAll && (tree eq null)

    def isContiguous = if (belowAll) {
      tree match {
        case a: Leaf => a.sign
        case null    => true
        case _       => false
      }
    } else {
      tree match {
        case _: Leaf                        => true
        case Branch(_, _, a: Leaf, b: Leaf) => a.sign & b.sign
        case null                           => true
        case _                              => false
      }
    }

    def hull: Interval[T] = {
      @tailrec
      def lowerBound(a: Tree): Bound[T] = a match {
        case a: Branch => lowerBound(a.left)
        case Above(x)  => Open(ise.fromLong(x))
        case Below(x)  => Closed(ise.fromLong(x))
        case Both(x)   => Closed(ise.fromLong(x))
        case _         => sys.error("no")
      }
      @tailrec
      def upperBound(a: Tree): Bound[T] = a match {
        case a: Branch => upperBound(a.right)
        case Both(x)   => Closed(ise.fromLong(x))
        case Above(x)  => Closed(ise.fromLong(x))
        case Below(x)  => Open(ise.fromLong(x))
        case _         => sys.error("no")
      }
      if (isEmpty) {
        Interval.empty[T]
      } else {
        val lower = if (belowAll) Unbound[T]() else lowerBound(tree)
        val upper = if (aboveAll) Unbound[T]() else upperBound(tree)
        Interval.fromBounds(lower, upper)
      }
    }

    def below(value: T): Boolean = SampleBelow(belowAll, tree, toPrefix(ise.toLong(value)))

    def at(value: T): Boolean = SampleAt(belowAll, tree, toPrefix(ise.toLong(value)))

    def above(value: T): Boolean = SampleAbove(belowAll, tree, toPrefix(ise.toLong(value)))

    def apply(value: T): Boolean = at(value)

    def &(rhs: IntervalTrie[T]) = rhs match {
      case rhs: IntervalTrieImpl[T] =>
        IntervalTrie[T](lhs.belowAll & rhs.belowAll, AndCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree))
    }

    def |(rhs: IntervalTrie[T]) = rhs match {
      case rhs: IntervalTrieImpl[T] =>
        IntervalTrie[T](lhs.belowAll | rhs.belowAll, OrCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree))
    }

    def ^(rhs: IntervalTrie[T]) = rhs match {
      case rhs: IntervalTrieImpl[T] =>
        IntervalTrie[T](lhs.belowAll ^ rhs.belowAll, XorCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree))
    }

    def unary_~ = IntervalTrie[T](!belowAll, tree)

    def isSupersetOf(rhs: IntervalTrie[T]) = rhs match {
      case rhs: IntervalTrieImpl[T] =>
        SupersetOfCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree)
    }

    def intersects(rhs: IntervalTrie[T]) = rhs match {
      case rhs: IntervalTrieImpl[T] =>
        !DisjointCalculator(lhs.belowAll, lhs.tree, rhs.belowAll, rhs.tree)
    }

    def isProperSupersetOf(rhs: IntervalTrie[T]) = isSupersetOf(rhs) && (rhs != lhs)

    def intervals = new Iterable[Interval[T]] {
      import scala.collection.mutable.ArrayBuffer

      override def foreach[U](f: Interval[T] => U): Unit = foreachInterval(belowAll, tree)(f)

      def iterator: Iterator[Interval[T]] = {
        val iseq: ArrayBuffer[Interval[T]] = ArrayBuffer.empty[Interval[T]]
        def f(i: Interval[T]): Unit = {
          iseq += i
        }
        foreachInterval(belowAll, tree)(f)
        iseq.iterator
      }
    }

    def intervalIterator = new IntervalIterator[T](lhs)

    def edges: Iterable[T] = new Iterable[T] {

      override def iterator: Iterator[T] = new EdgeIterator[T](lhs.tree)
    }

    override def toString = {
      if (isEmpty)
        Interval.empty[T].toString
      else
        intervals.map(_.toString).mkString(";")
    }
  }

}

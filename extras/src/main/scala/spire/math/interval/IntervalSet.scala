package spire.math.interval

import language.implicitConversions
import java.util.Arrays
import spire.algebra.{Bool, Eq, Order}
import spire.math.{Rational, Interval}
import scala.annotation.tailrec
import scala.reflect.ClassTag

final class IntervalSet[T] private (
    val belowAll: Boolean,
    private val values: Array[T],
    private val kinds: Array[Byte],
    private implicit val order: Order[T]) { lhs =>

  import IntervalSet._

  private def binarySearch(key: T): Int = {
    var low = 0
    var high = values.length - 1
    while (low <= high) {
      val mid = (low + high) >>> 1
      val midVal = values(mid)
      val c = order.compare(midVal, key)
      if (c < 0) {
        low = mid + 1
      }
      else if (c > 0) {
        high = mid - 1
      }
      else {
        // scalastyle:off return
        return mid
        // scalastyle:on return
      }
    }
    -(low + 1)
  }

  def below(index: Int): Boolean = {
    if (index == 0)
      belowAll
    else
      valueAbove(kinds(index - 1))
  }

  def at(value: T): Boolean = {
    val index = binarySearch(value)
    if (index >= 0)
      valueAt(kinds(index))
    else
      below(-index - 1)
  }

  def above(value: T): Boolean = {
    val index = binarySearch(value)
    if (index >= 0)
      valueAbove(kinds(index))
    else
      below(-index - 1)
  }

  def below(value: T): Boolean = {
    val index = binarySearch(value)
    if (index > 0)
      valueAbove(kinds(index - 1))
    else if (index == 0)
      belowAll
    else
      below(-index - 1)
  }

  def apply(value:T) = at(value)

  def aboveAll = if (values.isEmpty) belowAll else valueAbove(kinds.last)

  def unary_~ :IntervalSet[T] = copy(belowAll = !belowAll, kinds = negateKinds(kinds))

  def |(rhs:IntervalSet[T]) = new Or[T](lhs, rhs).result

  def &(rhs:IntervalSet[T]) = new And[T](lhs, rhs).result

  def ^(rhs:IntervalSet[T]) = new Xor[T](lhs, rhs).result

  def intersects(rhs: IntervalSet[T]): Boolean = !new Disjoint[T](lhs, rhs).result

  def isSupersetOf(rhs: IntervalSet[T]): Boolean = new IsSupersetOf[T](lhs, rhs).result

  def isProperSupersetOf(rhs: IntervalSet[T]): Boolean = isSupersetOf(rhs) && (lhs != rhs)

  private def copy(belowAll:Boolean = belowAll, values:Array[T] = values, kinds:Array[Byte] = kinds) =
    new IntervalSet[T](belowAll, values, kinds, order)

  override def toString = {
    if(isEmpty)
      Interval.empty[T].toString()
    else
      intervals.mkString(";")
  }

  override def hashCode = {
    belowAll.## * 41 + Arrays.hashCode(kinds) * 23 + Arrays.hashCode(values.asInstanceOf[Array[AnyRef]])
  }

  override def equals(rhs:Any) = rhs match {
    case rhs:IntervalSet[_] =>
      lhs.belowAll == rhs.belowAll &&
        Arrays.equals(lhs.kinds, rhs.kinds) &&
        Arrays.equals(values.asInstanceOf[Array[AnyRef]], rhs.values.asInstanceOf[Array[AnyRef]])
    case _ => false
  }

  def edges: Iterable[T] = values

  def isEmpty: Boolean = !belowAll && values.isEmpty

  def isContiguous: Boolean =
    if(belowAll) {
      kinds match {
        case Array() => true
        case Array(kind) => kind != K01
        case _ => false
      }
    } else {
      kinds match {
        case Array() => true
        case Array(_) => true
        case Array(a,b) => a != K10 && b != K01
        case _ => false
      }
    }

  private[this] def lowerBound(i:Int) = kinds(i) match {
    case K01 => Open(values(i))
    case K11 => Closed(values(i))
    case K10 => Closed(values(i))
    case _ => wrong
  }

  private[this] def upperBound(i:Int) = kinds(i) match {
    case K10 => Closed(values(i))
    case K00 => Open(values(i))
    case _ => wrong
  }

  def hull: Interval[T] = {
    if(isEmpty) {
      Interval.empty[T]
    } else if (belowAll && aboveAll) {
      Interval.all[T]
    } else if (belowAll) {
      Interval.fromBounds(Unbound(), upperBound(kinds.length - 1))
    } else if (aboveAll) {
      Interval.fromBounds(lowerBound(0), Unbound())
    } else {
      Interval.fromBounds(lowerBound(0), upperBound(kinds.length - 1))
    }
  }

  // todo: switch to AbstractTraversable once we no longer need to support scala 2.10
  def intervals: Traversable[Interval[T]] = new Traversable[Interval[T]] {
    override def foreach[U](f: (Interval[T]) => U): Unit = foreachInterval(f)
  }

  def intervalIterator: Iterator[Interval[T]] = new IntervalIterator[T](lhs)

  private def foreachInterval[U](f:Interval[T] => U) : Unit = {
    var prev: Option[Bound[T]] = if(belowAll) Some(Unbound()) else None
    for(i<-values.indices) {
      val vi = values(i)
      val ki = kinds(i)
      prev = (ki, prev) match {
        case (K00, Some(prev)) =>
          f(Interval.fromBounds(prev, Open(vi)))
          None
        case (K01, Some(prev)) =>
          f(Interval.fromBounds(prev, Open(vi)))
          Some(Open(vi))
        case (K10, Some(prev)) =>
          f(Interval.fromBounds(prev, Closed(vi)))
          None
        case (K10, None) =>
          f(Interval.fromBounds(Closed(vi),Closed(vi)))
          None
        case (K11, None) =>
          Some(Closed(vi))
        case (K01, None) =>
          Some(Open(vi))
        // $COVERAGE-OFF$
        case _ => wrong
        // $COVERAGE-ON$
      }
    }
    for(prev <- prev)
      f(Interval.fromBounds(prev, Unbound()))
  }

  private[interval] def kindsAccessor = kinds
}

object IntervalSet {

  def atOrAbove[T: Order](value: T) = singleton(false, value, K11)

  def above[T: Order](value: T) = singleton(false, value, K01)

  def atOrBelow[T: Order](value: T) = singleton(true, value, K10)

  def below[T: Order](value: T) = singleton(true, value, K00)

  def point[T: Order](value: T) = singleton(false, value, K10)

  def hole[T: Order](value: T) = singleton(true, value, K01)

  def empty[T: Order]: IntervalSet[T] = new IntervalSet[T](false, Array()(classTag), Array(), implicitly[Order[T]])

  def all[T: Order]: IntervalSet[T] = new IntervalSet[T](true, Array()(classTag), Array(), implicitly[Order[T]])

  implicit def apply[T: Order](value: Boolean) : IntervalSet[T] = new IntervalSet[T](value, Array()(classTag), Array(), implicitly[Order[T]])

  implicit def apply[T: Order](interval: Interval[T]): IntervalSet[T] = interval.fold {
    case (Closed(a),    Closed(b)) if a == b => point(a)
    case (Unbound(),    Open(x))      => below(x)
    case (Unbound(),    Closed(x))    => atOrBelow(x)
    case (Open(x),      Unbound())    => above(x)
    case (Closed(x),    Unbound())    => atOrAbove(x)
    case (Closed(a),    Closed(b))    => fromTo(a, K11, b, K10)
    case (Closed(a),    Open(b))      => fromTo(a, K11, b, K00)
    case (Open(a),      Closed(b))    => fromTo(a, K01, b, K10)
    case (Open(a),      Open(b))      => fromTo(a, K01, b, K00)
    case (Unbound(),    Unbound())    => all[T]
    case (EmptyBound(), EmptyBound()) => empty[T]
  }

  def apply(text:String) : IntervalSet[Rational] = {
    val intervals = text.split(';').map(Interval.apply)
    def intervalToIntervalSet(i:Interval[Rational]) : IntervalSet[Rational] = apply(i)
    val simpleSets = intervals.map(intervalToIntervalSet)
    (empty[Rational] /: simpleSets)(_ | _)
  }

  private def fromTo[T: Order](a:T, ak:Byte, b:T, bk:Byte) =
    new IntervalSet[T](false, Array(a,b)(classTag), Array(ak,bk), implicitly[Order[T]])

  private def wrong : Nothing = throw new IllegalStateException("")

  private def singleton[T: Order](belowAll: Boolean, value: T, kind: Byte): IntervalSet[T] =
    new IntervalSet(belowAll, Array(value)(classTag), Array(kind), implicitly[Order[T]])

  private val K00 = 0.toByte

  private val K10 = 1.toByte

  private val K01 = 2.toByte

  private val K11 = 3.toByte

  private def classTag[T] = ClassTag.AnyRef.asInstanceOf[ClassTag[T]]

  private def negateKind(kind: Byte) = ((~kind) & 3).toByte

  private def valueAt(kind: Byte): Boolean = (kind & 1) != 0

  private def valueAbove(kind: Byte): Boolean = (kind & 2) != 0

  private def negateKinds(kinds:Array[Byte]): Array[Byte] = {
    var i = 0
    val result = new Array[Byte](kinds.length)
    while(i < kinds.length) {
      result(i) = negateKind(kinds(i))
      i += 1
    }
    result
  }

  private abstract class MergeOperation[T] {

    def lhs:IntervalSet[T]

    def rhs:IntervalSet[T]

    private[this] val a0 = lhs.belowAll

    private[this] val b0 = rhs.belowAll

    private[this] val r0 = op(a0, b0)

    private[this] val a = lhs.values

    private[this] val b = rhs.values

    private[this] val ak = lhs.kinds

    private[this] val bk = rhs.kinds

    private[this] val order = lhs.order

    private[this] val r = Array.ofDim[T](a.length + b.length)(classTag)

    private[this] val rk = new Array[Byte](a.length + b.length)

    private[this] var ri = 0

    def copyA(a0: Int, a1: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      System.arraycopy(ak, a0, rk, ri, a1 - a0)
      ri += a1 - a0
    }

    def flipA(a0: Int, a1: Int): Unit = {
      System.arraycopy(a, a0, r, ri, a1 - a0)
      var ai = a0
      while(ai < a1) {
        rk(ri) = negateKind(ak(ai))
        ri += 1
        ai += 1
      }
    }

    def copyB(b0: Int, b1: Int): Unit = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      System.arraycopy(bk, b0, rk, ri, b1 - b0)
      ri += b1 - b0
    }

    def flipB(b0: Int, b1: Int): Unit = {
      System.arraycopy(b, b0, r, ri, b1 - b0)
      var bi = b0
      while(bi < b1) {
        rk(ri) = negateKind(bk(bi))
        ri += 1
        bi += 1
      }
    }

    def op(a:Boolean, b:Boolean) : Boolean

    def op(a:Byte, b:Byte) : Int

    def collision(ai: Int, bi: Int): Unit = {
      val kind = op(ak(ai), bk(bi)).toByte
      val below = rBelow
      if((below && kind != K11) || (!below && kind != K00)) {
        rk(ri) = kind
        r(ri) = a(ai)
        ri += 1
      }
    }

    def fromA(a0:Int, a1: Int, b:Boolean) : Unit

    def fromB(a:Boolean, b0:Int, b1: Int) : Unit

    protected[this] def valueAbove(kind: Byte): Boolean = (kind & 2) != 0

    protected[this] def aBelow(i:Int) = if(i>0) valueAbove(ak(i-1)) else a0

    protected[this] def bBelow(i:Int) = if(i>0) valueAbove(bk(i-1)) else b0

    protected[this] def rBelow = if(ri > 0) valueAbove(rk(ri-1)) else r0

    def binarySearch(array: Array[T], key: T, from: Int, until: Int): Int = {
      var low = from
      var high = until - 1
      while (low <= high) {
        val mid = (low + high) >>> 1
        val midVal = array(mid)
        val c = order.compare(midVal, key)
        if (c < 0) {
          low = mid + 1
        }
        else if (c > 0) {
          high = mid - 1
        }
        else {
          // scalastyle:off return
          return mid
          // scalastyle:on return
        }
      }
      -(low + 1)
    }

    def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
      if (a0 == a1) {
        fromB(aBelow(a0), b0, b1)
      } else if (b0 == b1) {
        fromA(a0, a1, bBelow(b0))
      } else {
        val am = (a0 + a1) / 2
        val res = binarySearch(b, a(am), b0, b1)
        if (res >= 0) {
          // same elements
          val bm = res
          // merge everything below a(am) with everything below the found element
          merge0(a0, am, b0, bm)
          // add the elements a(am) and b(bm)
          collision(am, bm)
          // merge everything above a(am) with everything above the found element
          merge0(am + 1, a1, bm + 1, b1)
        } else {
          val bm = -res - 1
          // merge everything below a(am) with everything below the found insertion point
          merge0(a0, am, b0, bm)
          // add a(am)
          fromA(am, am + 1, bBelow(bm))
          // everything above a(am) with everything above the found insertion point
          merge0(am + 1, a1, bm, b1)
        }
      }
    }

    merge0(0, a.length, 0, b.length)

    def result : IntervalSet[T] = {
      if(ri == r.length)
        new IntervalSet(r0, r, rk, order)
      else
        new IntervalSet(r0, r.take(ri), rk.take(ri), order)
    }
  }

  private class And[T](val lhs:IntervalSet[T], val rhs:IntervalSet[T]) extends MergeOperation[T] {

    override def op(a: Boolean, b: Boolean): Boolean = a & b

    override def op(a: Byte, b: Byte): Int = a & b

    override def fromA(a0: Int, a1: Int, b: Boolean): Unit =
      if(b)
        copyA(a0,a1)

    override def fromB(a: Boolean, b0: Int, b1: Int): Unit =
      if(a)
        copyB(b0,b1)
  }

  private class Or[T](val lhs:IntervalSet[T], val rhs:IntervalSet[T]) extends MergeOperation[T] {

    override def op(a: Boolean, b: Boolean): Boolean = a | b

    override def op(a: Byte, b: Byte): Int = a | b

    override def fromA(a0: Int, a1: Int, b: Boolean): Unit =
      if(!b)
        copyA(a0,a1)

    override def fromB(a: Boolean, b0: Int, b1: Int): Unit =
      if(!a)
        copyB(b0,b1)
  }

  private class Xor[T](val lhs:IntervalSet[T], val rhs:IntervalSet[T]) extends MergeOperation[T] {

    override def op(a: Boolean, b: Boolean): Boolean = a ^ b

    override def op(a: Byte, b: Byte): Int = a ^ b

    override def fromA(a0: Int, a1: Int, b: Boolean): Unit =
      if(!b)
        copyA(a0,a1)
      else
        flipA(a0,a1)

    override def fromB(a: Boolean, b0: Int, b1: Int): Unit =
      if(!a)
        copyB(b0,b1)
      else
        flipB(b0,b1)
  }

  private abstract class BooleanOperation[T] {

    def lhs:IntervalSet[T]

    def rhs:IntervalSet[T]

    private[this] val a0 = lhs.belowAll

    private[this] val b0 = rhs.belowAll

    private[this] val a = lhs.values

    private[this] val b = rhs.values

    private[this] val ak = lhs.kinds

    private[this] val bk = rhs.kinds

    private[this] val order = lhs.order

    protected[this] def op(a:Boolean, b:Boolean) : Boolean

    protected[this] def op(a:Byte, b:Byte) : Boolean

    protected[this] def collision(ai: Int, bi: Int): Boolean =
      op(ak(ai), bk(bi))

    protected[this] def fromA(a0:Int, a1: Int, b:Boolean) : Boolean

    protected[this] def fromB(a:Boolean, b0:Int, b1: Int) : Boolean

    protected[this] def valueAbove(kind: Byte): Boolean = (kind & 2) != 0

    protected[this] def aBelow(i:Int) = if(i>0) valueAbove(ak(i-1)) else a0

    protected[this] def bBelow(i:Int) = if(i>0) valueAbove(bk(i-1)) else b0

    def binarySearch(array: Array[T], key: T, from: Int, until: Int): Int = {
      var low = from
      var high = until - 1
      while (low <= high) {
        val mid = (low + high) >>> 1
        val midVal = array(mid)
        val c = order.compare(midVal, key)
        if (c < 0) {
          low = mid + 1
        }
        else if (c > 0) {
          high = mid - 1
        }
        else {
          // scalastyle:off return
          return mid
          // scalastyle:on return
        }
      }
      -(low + 1)
    }

    def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Boolean = {
      if(a0 == a1 && b0 == b1) {
        true
      } else if (a0 == a1) {
        fromB(aBelow(a0), b0, b1)
      } else if (b0 == b1) {
        fromA(a0, a1, bBelow(b0))
      } else {
        val am = (a0 + a1) / 2
        val res = binarySearch(b, a(am), b0, b1)
        if (res >= 0) {
          // same elements
          val bm = res
          // merge everything below a(am) with everything below the found element
          merge0(a0, am, b0, bm) &&
            // add the elements a(am) and b(bm)
            collision(am, bm) &&
            // merge everything above a(am) with everything above the found element
            merge0(am + 1, a1, bm + 1, b1)
        } else {
          val bm = -res - 1
          // merge everything below a(am) with everything below the found insertion point
          merge0(a0, am, b0, bm) &&
            // add a(am)
            fromA(am, am + 1, bBelow(bm)) &&
            // everything above a(am) with everything above the found insertion point
            merge0(am + 1, a1, bm, b1)
        }
      }
    }

    val result = op(a0, b0) && merge0(0, a.length, 0, b.length)
  }

  private class IsSupersetOf[T](val lhs:IntervalSet[T], val rhs:IntervalSet[T]) extends BooleanOperation[T] {

    override def op(a: Boolean, b: Boolean): Boolean = a | !b

    override def op(a: Byte, b: Byte): Boolean = ((a | ~b) & 3) == 3

    override def fromA(a0: Int, a1: Int, b: Boolean): Boolean = !b

    override def fromB(a: Boolean, b0: Int, b1: Int): Boolean = a
  }

  private class Disjoint[T](val lhs:IntervalSet[T], val rhs:IntervalSet[T]) extends BooleanOperation[T] {

    override def op(a: Boolean, b: Boolean): Boolean = !(a & b)

    override def op(a: Byte, b: Byte): Boolean = (a & b) == 0

    override def fromA(a0: Int, a1: Int, b: Boolean): Boolean = !b

    override def fromB(a: Boolean, b0: Int, b1: Int): Boolean = !a
  }

  // todo: switch to AbstractIterator once we no longer need to support 2.10
  private final class IntervalIterator[T:Order](s: IntervalSet[T]) extends Iterator[Interval[T]] {

    private[this] val values = s.values

    private[this] val kinds = s.kinds

    private[this] var lower: Bound[T] = if (s.belowAll) Unbound() else null

    private[this] var i = 0

    private[this] def nextInterval() = {
      var result: Interval[T] = null
      if (i < kinds.length) {
        val kind = kinds(i)
        val value = values(i)
        i += 1
        if (lower eq null) kind match {
          case K10 =>
            result = Interval.point(value)
            lower = null
          case K11 =>
            result = null
            lower = Closed(value)
          case K01 =>
            result = null
            lower = Open(value)
          case _ => wrong
        } else kind match {
          case K01 =>
            val upper = Open(value)
            result = Interval.fromBounds[T](lower, upper)
            lower = upper
          case K00 =>
            val upper = Open(value)
            result = Interval.fromBounds[T](lower, upper)
            lower = null
          case K10 =>
            val upper = Closed(value)
            result = Interval.fromBounds[T](lower, upper)
            lower = null
          case _ => wrong
        }
      } else if (lower ne null) {
        result = Interval.fromBounds(lower, Unbound())
        lower = null
      } else {
        Iterator.empty.next()
      }
      result
    }

    def hasNext = (i < kinds.length) || (lower ne null)

    @tailrec
    override def next(): Interval[T] = {
      val result = nextInterval()
      if (result ne null)
        result
      else
        next()
    }
  }
}

object IntervalSetAlgebra {

  implicit def booleanAlgebra[T:Order] = new Bool[IntervalSet[T]] with Eq[IntervalSet[T]] {

    def eqv(x: IntervalSet[T], y: IntervalSet[T]) = x == y

    def zero = IntervalSet.empty[T]

    def one = IntervalSet.all[T]

    def complement(a: IntervalSet[T]) = ~a

    def or(a: IntervalSet[T], b: IntervalSet[T]) = a | b

    def and(a: IntervalSet[T], b: IntervalSet[T]) = a & b

    override def xor(a: IntervalSet[T], b: IntervalSet[T]) = a ^ b
  }
}

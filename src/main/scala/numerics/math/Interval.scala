package numerics.math

import Implicits._

// TODO: finish defining operations, especially division
// consider whether to restrict T to Fractional or Field instead of Numeric.
// (integral intervals behave very differently, e.g. division).

// TODO: maybe split Unbound into UnboundBelow and UnboundAbove. this would
// simplify some of the confusing code and make the comparators more honest.

sealed trait Bound[T] {
  val num:Fractional[T]

  // NOTE: these comparators are a bit weird and should maybe be renamed?
  def <(rhs:Bound[T]):Boolean
  def <=(rhs:Bound[T]):Boolean
  def >(rhs:Bound[T]):Boolean
  def >=(rhs:Bound[T]):Boolean

  def min(rhs:Bound[T]):Bound[T]
  def max(rhs:Bound[T]):Bound[T]

  def isZero:Boolean
  def isAboveZero:Boolean
  def isAtOrAboveZero:Boolean
  def isBelowZero:Boolean
  def isAtOrBelowZero:Boolean

  def n:T

  def unop(f:Function1[T, T]):Bound[T]
  def abs:Bound[T] = unop(t => num.abs(t))
  def negate:Bound[T] = unop(t => num.negate(t))

  def binop(rhs:Bound[T], f:Function2[T, T, T]):Bound[T]
  def +(rhs:Bound[T]) = binop(rhs, (x:T, y:T) => num.plus(x, y))
  def -(rhs:Bound[T]) = binop(rhs, (x:T, y:T) => num.minus(x, y))
  def *(rhs:Bound[T]) = binop(rhs, (x:T, y:T) => num.times(x, y))
  def /(rhs:Bound[T]) = binop(rhs, (x:T, y:T) => num.div(x, y))
}

//case class UnboundBelow[T:Fractional]() extends Bound[T] {
//  val num = implicitly[Fractional[T]]
//
//  def <(rhs:Bound[T]) = true
//  def <=(rhs:Bound[T]) = true
//  def >(rhs:Bound[T]) = false
//  def >=(rhs:Bound[T]) = false
//
//  def isZero = false
//  def isAboveZero = false
//  def isAtOrAboveZero = false
//  def isBelowZero = true
//  def isAtOrBelowZero = true
//
//  def n = throw new Exception("Unbound has no boundary value")
//
//  def unop(f:Function1[T, T]) = UnboundBelow()
//  def binop(rhs:Bound[T], f:Function2[T, T, T]) = rhs match {
//    case UnboundAbove() => error("undefined")
//    case _ => UnboundBelow()
//  }
//
//  def min(rhs:Bound[T]) = UnboundBelow()
//  def max(rhs:Bound[T]) = rhs
//}

case class Unbound[T:Fractional]() extends Bound[T] {
  val num = implicitly[Fractional[T]]

  def <(rhs:Bound[T]) = true
  def <=(rhs:Bound[T]) = true
  def >(rhs:Bound[T]) = true
  def >=(rhs:Bound[T]) = true

  def isZero = false
  def isAboveZero = true
  def isAtOrAboveZero = true
  def isBelowZero = true
  def isAtOrBelowZero = true

  def n = throw new Exception("Unbound has no boundary value")

  def unop(f:Function1[T, T]) = Unbound()
  def binop(rhs:Bound[T], f:Function2[T, T, T]) = Unbound()

  def min(rhs:Bound[T]) = Unbound()
  def max(rhs:Bound[T]) = Unbound()
}

case class OpenBound[T:Fractional](n:T) extends Bound[T] {
  val num = implicitly[Fractional[T]]

  def <(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case _ => n < rhs.n
  }
  def <=(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case OpenBound(m) => n <= m
    case ClosedBound(m) => n < m
  }
  def >(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case _ => n > rhs.n
  }
  def >=(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case OpenBound(m) => n >= m
    case ClosedBound(m) => n > m
  }

  def isZero = n == num.zero
  def isAboveZero = n >= num.zero
  def isAtOrAboveZero = n >= num.zero
  def isBelowZero = n <= num.zero
  def isAtOrBelowZero = n <= num.zero

  def unop(f:Function1[T, T]) = OpenBound(f(n))
  def binop(rhs:Bound[T], f:Function2[T, T, T]) = rhs match {
    case Unbound() => Unbound()
    case _ => OpenBound(f(n, rhs.n))
  }

  def min(rhs:Bound[T]) = rhs match {
    case Unbound() => rhs
    case _ => if (n < rhs.n) this else rhs
  }
  def max(rhs:Bound[T]) = rhs match {
    case Unbound() => rhs
    case _ => if (n > rhs.n) this else rhs
  }
}

case class ClosedBound[T:Fractional](n:T) extends Bound[T] {
  val num = implicitly[Fractional[T]]

  def <(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case _ => n < rhs.n
  }
  def <=(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case _ => n <= rhs.n
  }
  def >(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case _ => n > rhs.n
  }
  def >=(rhs:Bound[T]) = rhs match {
    case Unbound() => false
    case _ => n >= rhs.n
  }

  def isZero = n == num.zero
  def isAboveZero = n > num.zero
  def isAtOrAboveZero = n >= num.zero
  def isBelowZero = n < num.zero
  def isAtOrBelowZero = n <= num.zero

  def unop(f:Function1[T, T]) = ClosedBound(f(n))
  def binop(rhs:Bound[T], f:Function2[T, T, T]):Bound[T] = rhs match {
    case Unbound() => Unbound()
    case OpenBound(m) => OpenBound(f(n, m))
    case ClosedBound(m) => ClosedBound(f(n, m))
  }

  def min(rhs:Bound[T]) = rhs match {
    case Unbound() => rhs
    case _ => if (n <= rhs.n) this else rhs
  }
  def max(rhs:Bound[T]) = rhs match {
    case Unbound() => rhs
    case _ => if (n >= rhs.n) this else rhs
  }
}

case class Interval[T:Fractional](lower:Bound[T], upper:Bound[T]) {
  assert (lower <= upper)

  val num = implicitly[Fractional[T]]

  def isPositive = !lower.isAtOrBelowZero
  def isNegative = !lower.isAtOrAboveZero
  def isZero = lower.isZero && upper.isZero
  def containsZero = lower.isAtOrBelowZero && upper.isAtOrAboveZero
  def crossesZero = lower.isBelowZero && upper.isAboveZero

  def splitAtZero = if (lower.isAtOrAboveZero) {
    // only positive
  } else if (upper.isAtOrBelowZero) {
    // only negative
  } else {
    
  }
  
  def abs = {
    val a = lower.abs
    val b = upper.abs
    if (crossesZero) {
      Interval(ClosedBound(num.zero), a max b)
    } else if (a < b) {
      Interval(a, b)
    } else {
      Interval(b, a)
    }
  }
  
  def +(rhs:Interval[T]) = Interval(lower + rhs.lower, upper + rhs.upper)
  def -(rhs:Interval[T]) = Interval(lower - rhs.lower, upper - rhs.upper)

  def *(rhs:Interval[T]):Interval[T] = {
    val tcz = crossesZero
    val rcz = rhs.crossesZero
  
    val ll = lower * rhs.lower
    val lu = lower * rhs.upper
    val ul = upper * rhs.lower
    val uu = upper * rhs.upper
  
    if (tcz && rcz) {
      Interval(lu min ul, ll max uu)
    } else if (tcz) {
      Interval(ll min lu, ul max uu)
    } else if (rcz) {
      Interval(ll min ul, lu max uu)
    } else if (isNegative == rhs.isNegative) {
      Interval(ll min uu, ll max uu)
    } else {
      Interval(lu min ul, lu max ul)
    }
  }

  def /(rhs:Interval[T]) = {
    if (rhs.isZero) throw new Exception("/ by interval containing only 0")

    sys.error("todo")
  }
}

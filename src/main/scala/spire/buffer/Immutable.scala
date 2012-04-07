package spire.buffer

import scala.math.{min, max}
import scala.{specialized => spec}

object Immutable {
  def safe[@spec A:Manifest](as:Array[A]) = new Immutable(as.clone, 0, as.length)
  def unsafe[@spec A:Manifest](as:Array[A]) = new Immutable(as, 0, as.length)
  def apply[@spec A:Manifest](as:Array[A]) = unsafe(as)
  def empty[@spec A:Manifest] = unsafe(Array.empty[A])
  def ofDim[@spec A:Manifest](n:Int) = unsafe(Array.ofDim[A](n))
  def fill[@spec A:Manifest](n:Int)(a:A) = unsafe(Array.fill(n)(a))
}

final class Immutable[@spec A:Manifest](elems:Array[A], start:Int, end:Int) extends Buffer[A] {
  def length = end - start
  def toArray = Buffer.alloc(elems, start, length)
  def slice(i:Int, j:Int) = new Immutable(elems, start + i, start + j)
  def apply(i:Int) = elems(start + i)
  def get(i:Int) = if (i < length) Some(elems(start + i)) else None

  def toImmutable = this
  def toImmutableUnsafe = this

  def toMutable = Mutable.unsafe(toArray)
  def toMutableUnsafe = Mutable.unsafe(toArray)
}

final class Reversed[@spec A:Manifest](elems:Array[A], start:Int, end:Int) extends Buffer[A] {
  @inline final def index(i:Int) = end - 1 - i
  def length = end - start
  def toArray = {
    val as = Array.ofDim[A](length)
    var i = index(0)
    var j = 0
    while (i >= start) {
      as(j) = elems(i)
      i -= 1
      j += 1
    }
    as
  }
  def slice(i:Int, j:Int) = new Reversed(elems, start + length - j, end - i)
  def apply(i:Int) = elems(index(i))
  def get(i:Int) = if (i < length) Some(elems(index(i))) else None

  def toImmutable = Immutable.unsafe(toArray)
  def toImmutableUnsafe = Immutable.unsafe(toArray)

  def toMutable = Mutable.unsafe(toArray)
  def toMutableUnsafe = Mutable.unsafe(toArray)
}
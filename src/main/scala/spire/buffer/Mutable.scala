package spire.buffer

import scala.math.{min, max}
import scala.{specialized => spec}

object Mutable {
  def safe[@spec A:Manifest](as:Array[A]) = new Mutable(as.clone, as.length)
  def unsafe[@spec A:Manifest](as:Array[A]) = new Mutable(as, as.length)
  def apply[@spec A:Manifest](as:Array[A]) = unsafe(as)
  def empty[@spec A:Manifest] = unsafe(Array.empty[A])
  def ofDim[@spec A:Manifest](n:Int) = unsafe(Array.ofDim[A](n))
  def fill[@spec A:Manifest](n:Int)(a:A) = unsafe(Array.fill(n)(a))
}

final class Mutable[@spec A:Manifest](as:Array[A], n:Int) extends Buffer[A] {
  protected[this] var elems:Array[A] = as
  protected[this] var len:Int = n

  def length = len
  def toArray = Buffer.alloc(elems, 0, len)
  def slice(i:Int, j:Int) = Mutable.unsafe(Buffer.alloc(elems, i, j - i))
  def reverse = Mutable.unsafe(as.reverse)
  def map[@spec B:Manifest](f:A => B) = Mutable.unsafe(as.map(f))
  def apply(i:Int) = elems(i)
  def get(i:Int) = if (i < len) Some(elems(i)) else None
  def update(i:Int, a:A):Unit = elems(i) = a
  def toImmutable = Immutable(as.clone)
  def toImmutableUnsafe = Immutable(as)
  def toMutable = new Mutable(as.clone, n)
  def toMutableUnsafe = this

  protected[this] def resizeIfNecessary(n:Int):Unit = {
    val x = elems.length
    if (len + n > x) {
      val x2 = if (x < 4) 8 else if (x <= 0x3fffffff) x * 2 else Int.MaxValue
      val as = Array.ofDim[A](x2)
      Buffer.copy(elems, as, 0, 0, len)
      elems = as
    }
  }

  def append(a:A):Unit = insert(len, a)
  def prepend(a:A):Unit = insert(0, a)
  def insert(i:Int, a:A):Unit = {
    resizeIfNecessary(1)
    Buffer.rcopy(elems, elems, i, i + 1, len - i)
    elems(i) = a
    len += 1
  }

  def prextend(as:Array[A]):Unit = splice(0, as)
  def extend(as:Array[A]):Unit = splice(len, as)
  def splice(i:Int, as:Array[A]):Unit = {
    val n = as.length
    resizeIfNecessary(as.length)
    Buffer.rcopy(elems, elems, i, i + as.length, len - i)
    Buffer.copy(as, elems, 0, i, as.length)
    len += as.length
  }

  def remove(i:Int):Unit = {
    Buffer.copy(elems, elems, i + 1, i, len - i - 1)
    len -= 1
  }
  def pop(i:Int):A = {
    val a = elems(i)
    remove(i)
    a
  }
}

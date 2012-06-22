package spire.buffer

import scala.reflect.ClassTag
import scala.math.{min, max}
import scala.{specialized => spec}

import spire.buffer.Buffer.{alloc, mapBufferToArray}

object Immutable {
  def safe[@spec A:ClassTag](as:Array[A]) = new Forward(as.clone, 0, as.length)
  def unsafe[@spec A:ClassTag](as:Array[A]) = new Forward(as, 0, as.length)
  def apply[@spec A:ClassTag](as:Array[A]) = unsafe(as)
  def empty[@spec A:ClassTag] = unsafe(Array.empty[A])
  def ofDim[@spec A:ClassTag](n:Int) = unsafe(Array.ofDim[A](n))
  def fill[@spec A:ClassTag](n:Int)(a:A) = unsafe(Array.fill(n)(a))
}

trait Immutable[@spec A] extends Buffer[A] {
  implicit def manifest:ClassTag[A]
  def toImmutable = this
  def toImmutableUnsafe = this
  def toMutable = Mutable.unsafe(toArray)
  def toMutableUnsafe = Mutable.unsafe(toArray)
  def toLazy:Lazy[A, _] = new Lazy(this)(n => n)
  def slice(i:Int, j:Int):Immutable[A]
  def reverse:Immutable[A]
}

final class Forward[@spec A](elems:Array[A], start:Int, end:Int)(implicit val manifest:ClassTag[A]) extends Immutable[A] {
  def toArray = alloc(elems, start, length)
  def length = end - start
  def slice(i:Int, j:Int) = new Forward(elems, start + i, start + j)
  def reverse = new Reversed(elems, start, end)
  def apply(i:Int) = elems(start + i)
  def get(i:Int) = if (i < length) Some(elems(start + i)) else None
  def map[@spec B:ClassTag](f:A => B) = Immutable.unsafe(mapBufferToArray(this, start, length)(f))
}

final class Reversed[@spec A](elems:Array[A], start:Int, end:Int)(implicit val manifest:ClassTag[A]) extends Immutable[A] {
  @inline final def index(i:Int) = end - 1 - i
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

  def length = end - start
  def slice(i:Int, j:Int) = new Reversed(elems, start + length - j, end - i)
  def reverse = new Forward(elems, start, end)
  def apply(i:Int) = elems(index(i))
  def get(i:Int) = if (i < length) Some(elems(index(i))) else None
  def map[@spec B:ClassTag](f:A => B) = Immutable.unsafe(mapBufferToArray(this, start, length)(f))
}

final class Lazy[@spec A, @spec U](us:Immutable[U])(f:U => A)(implicit val manifest:ClassTag[A]) extends Immutable[A] {
  def length = us.length
  def toArray = mapBufferToArray(us, 0, length)(f)
  override def toLazy = this
  def slice(i:Int, j:Int) = new Lazy(us.slice(i, j))(f)
  def reverse = new Lazy(us.reverse)(f)
  def apply(i:Int) = f(us(i))
  def get(i:Int) = us.get(i).map(f)
  def map[@spec B:ClassTag](f:A => B) = new Lazy(us)(u => f(this.f(u)))
}

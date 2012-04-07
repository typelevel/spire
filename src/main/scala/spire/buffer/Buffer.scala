package spire.buffer

import scala.math.{min, max}
import scala.{specialized => spec}

object Buffer {
  def alloc[@spec A:Manifest](src:Array[A], s1:Int, len:Int) = {
    val as = Array.ofDim[A](len)
    Buffer.copy(src, as, s1, 0, len)
    as
  }

  def copy[@spec A](src:Array[A], dst:Array[A], s1:Int, d1:Int, len:Int) {
    var i = s1
    var j = d1
    var k = s1 + len
    while (i < k) {
      dst(j) = src(i)
      i += 1
      j += 1
    }
  }

  def rcopy[@spec A](src:Array[A], dst:Array[A], s1:Int, d1:Int, len:Int) {
    var i = s1 + len - 1
    var j = d1 + len - 1
    while (i >= s1) {
      dst(j) = src(i)
      i -= 1
      j -= 1
    }
  }

  //def empty[@spec A:Manifest] = new Buffer2(Array.empty[A], 0)
  //def ofDim[@spec A:Manifest](n:Int) = new Buffer2(Array.ofDim[A](n), 0)
  //def apply[@spec A:Manifest](elems:Array[A]) = new Buffer2(elems, elems.length)
}

trait Buffer[@spec A] {
  def toArray:Array[A]
  def length:Int

  def apply(i:Int):A
  def get(i:Int):Option[A]

  def slice(i:Int, j:Int):Buffer[A]

  override def toString = {
    val sb = new StringBuilder()
    sb.append("Buffer(")
    if (length > 0) {
      sb.append(apply(0))
      var i = 1
      while (i < length) {
        sb.append(",")
        sb.append(apply(i))
        i += 1
      }
    }
    sb.append(")")
    sb.toString
  }

  def toImmutable:Immutable[A]
  def toImmutableUnsafe:Immutable[A]

  def toMutable:Mutable[A]
  def toMutableUnsafe:Mutable[A]
}

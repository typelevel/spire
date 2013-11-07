package spire.matrix.dense
import spire.matrix.Constants._
import spire.syntax.cfor._

import scala.collection.mutable

/**
 * Simple vector abstraction
 */
trait VectorLike extends mutable.IndexedSeq[Double] {

  /** Assign the elements of other to this */
  def := (other:VectorLike): Unit = {
    cforRange(0 until length) { i => this(i) = other(i) }
  }

  /** Assign the given value to every elements of this */
  def := (e:Double): Unit = {
    cforRange(0 until length) { i => this(i) = e }
  }

  /** Assign the elements produced by the given iterator to this */
  def :=(other:Iterator[Double]):Unit = {
    cforRange(0 until length) { i => other.next }
  }

  /**
   * Reverse elements in-place and return this
   */
  def reverseInPlace:VectorLike = {
    cforRange(0 until length/2) { i =>
      val j = length - 1 - i
      val t = this(i); this(i) = this(j); this(j) = t
    }
    this
  }

  /**
   * The block V(first:end)
   *
   * If end is End, then it takes the value length.
   */
  def block(first:Int, end:Int) = {
    new VectorBlock(this, first, if(end == End) length else end)
  }

  def isZero = find(_ != 0) == None

  def copyToVector = new Vector(this.toArray)

  override def toString =
    formatted(StringFormatting.elementFormat,
              StringFormatting.useMathematicaFormat)

  def formatted(fmt: String, useMathematicaFormat: Boolean=false): String = {
    val (start, step, end) = StringFormatting.ofRows(useMathematicaFormat)
    val disp = this.map(fmt format _).mkString(start, step, end)
    StringFormatting.postprocess(disp, useMathematicaFormat)
  }
}

/**
 * A vector actually allocating elements
 */
class Vector(elems:Array[Double])
extends VectorLike {
  def this(n:Int) = this(new Array[Double](n))

  final def length = elems.size

  final def apply(i:Int): Double = elems(i)

  final def update(i:Int, value:Double) = { elems(i) = value }

  override def iterator = elems.toIterator

  override def foreach[U](f:Double => U) = elems.foreach(f)
}

/** Vector companion object */
object Vector {
  def apply(elements:Double*) = new Vector(elements.toArray)

  def zero(length:Int) = new Vector(new Array[Double](length))

  def empty(length:Int) = zero(length)
}

/**
 * Contiguous subset of elements in a vector
 *
 * @constructor Construct x(first:end)
 */
final
class VectorBlock(private val x:VectorLike, val first:Int, end:Int)
  extends VectorLike
{
  require(0 <= first && first <= end && end <= x.length)
  val length = end - first
  def apply(k:Int) = x(first + k)
  def update(k:Int, value:Double) = { x(first + k) = value }
  override def iterator = (
    for (k <- first until first + length) yield x(k)).toIterator
}
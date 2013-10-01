package spire.matrix.dense
import spire.matrix.Constants._
import scala.collection.mutable

/**
 * Simple vector abstraction
 */
trait VectorLike extends mutable.IndexedSeq[Double] {

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
    this.map("%10.3g" format _).mkString("[", ", ", "]")
}

/**
 * A vector actually allocating elements
 */
class Vector(elems:Array[Double])
extends VectorLike {
  def this(n:Int) = this(new Array[Double](n))

  def length = elems.size

  def apply(i:Int): Double = elems(i)

  def update(i:Int, value:Double) = { elems(i) = value }

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
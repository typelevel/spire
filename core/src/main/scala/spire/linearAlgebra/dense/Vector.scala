package spire.linearAlgebra.dense
import spire.linearAlgebra.Constants._

/**
 * Simple vector abstraction
 */
trait VectorLike extends IndexedSeq[Double] {
  def apply(i:Int): Double
  def update(i:Int, value:Double)
  val length: Int

  /**
   * The block V(first:end)
   *
   * If end is End, then it takes the value length.
   */
  def block(first:Int, end:Int) = {
    new VectorBlock(this, first, if(end == End) length else end)
  }

  def isZero = find(_ != 0) == None

  def copyToVector = new Vector(length, this.toArray)

  override def toString =
    this.map("%10.3g" format _).mkString("[", ", ", "]")
}

/**
 * A vector actually allocating elements
 */
class Vector(val length:Int, itsElements:Traversable[Double] = null)
extends VectorLike {
  require(itsElements == null || itsElements.size == length)

  protected var elems = if(itsElements != null) itsElements.toArray
                        else new Array[Double](length)

  def apply(i:Int): Double = elems(i)

  def update(i:Int, value:Double) = { elems(i) = value }

  override def iterator = elems.toIterator

  override def foreach[U](f:Double => U) = elems.foreach(f)
}

/** Vector companion object */
object Vector {
  def apply(elements:Double*) = new Vector(elements.size, elements)

  def zero(length:Int) = new Vector(length)

  lazy val empty = new Vector(0)
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
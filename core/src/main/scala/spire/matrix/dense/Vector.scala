package spire.matrix.dense
import spire.syntax.cfor._

/**
 * Simple vector abstraction
 *
 * The elements of a Vector are mutable but its dimension is immutable
 *
 * Vector elements are stored at regular intervals in the attribute `elements`.
 * As a result, this class can model not only a vector per se (for which there
 * is a one-to-one mapping between the vector elements and the elements of that
 * array) but also matrix rows, columns, and diagonals as well as vector blocks.
 *
 * @todo parametrise by the type of elements
 *
 * @param dimension is the number of elements
 * @param step is the distance between two successive elements of this vector
 *        in `elements`
 * @param start is the position of the first element of this vector
 *        in `elements`
 * @param elements stores the vector elements as explained above
 */
class Vector(val dimension:Int, val step:Int,
             val start:Int, val elements:Array[Double])
extends Iterable[Double] {

  /** Convenience constructor to quickly get a vector of a given dimension */
  def this(dimension:Int) = this(dimension, 1, 0, new Array[Double](dimension))

  /** i-th element (0-based) */
  final def apply(i:Int) = elements(start + i*step)

  /** Set i-th element (0-based) */
  final def update(i:Int, x:Double) { elements(start + i*step) = x }

  /** The block V(first:end) */
  final def block(first:Int, end:Int) =
    new Vector(end-first, step, start + first*step, elements)

  override def equals(other:Any): Boolean =
    other match {
      case that: Vector => dimension == that.dimension &&
                           (this sameElements that)
      case _ => false
    }

  /** Whether this has the same elements as other */
  def sameElements(that:Vector): Boolean = {
    cforRange(0 until dimension) { i =>
      if(this(i) != that(i)) return false
    }
    return true
  }

  def iterator =
    if(step == 1)
      new Iterator[Double] {
        var i = start
        def hasNext = i < start + dimension
        def next = {
          val elt = elements(i)
          i += 1
          elt
        }
      }
    else
      new Iterator[Double] {
        var i = start
        def hasNext = i < start + dimension*step
        def next = {
          val elt = elements(i)
          i += step
          elt
        }
      }

  /** Assign the elements of other to this */
  def := (other:Vector): Unit = {
    cforRange(0 until dimension) { i => this(i) = other(i) }
  }

  /** Assign the given value to every elements of this */
  def := (e:Double): Unit = {
    cforRange(0 until dimension) { i => this(i) = e }
  }

  /** Assign the elements produced by the given iterator to this */
  def :=(other:Iterator[Double]):Unit = {
    cforRange(0 until dimension) { i => this(i) = other.next }
  }

  def isZero = find(_ != 0) == None

  def copyToVector = new Vector(dimension, 1, 0, toArray)

  /**
   * Reverse elements in-place and return this
   */
  def reverseInPlace:Vector = {
    cforRange(0 until dimension/2) { i =>
      val j = dimension - 1 - i
      val t = this(i); this(i) = this(j); this(j) = t
    }
    this
  }

  override def toString =
    formatted(StringFormatting.elementFormat,
              StringFormatting.useMathematicaFormat)

  def formatted(fmt: String, useMathematicaFormat: Boolean=false): String = {
    val (start, step, end) = StringFormatting.ofRows(useMathematicaFormat)
    val disp = this.map(fmt format _).mkString(start, step, end)
    StringFormatting.postprocess(disp, useMathematicaFormat)
  }
}

/** Vector construction */
trait VectorConstruction[V <: Vector] {
  /** Construct a vector with the given elements */
  def apply(elements:Array[Double]): V

  /** Construct a vector with the given dimension and uninitialized elements */
  def empty(n:Int): V

  /** Construct a vector with the given elements */
  def apply(elements:Double*): V = this(elements.toArray)

  /** The zero vector of given dimension */
  def zero(n:Int) = this(new Array[Double](n))

  /**
   * Fill a vector of dimension n with the repeated evaluation of `element`
   */
  def fill(n:Int)(element: => Double) = {
    val vector = empty(n)
    cforRange(0 until n) { i => vector(i) = element }
    vector
  }

  /** Fill a vector of dimension n such that the i-th element is f(i) */
  def tabulate(n:Int)(f: (Int) => Double) = {
    val vector = empty(n)
    cforRange(0 until n) { i => vector(i) = f(i) }
    vector
  }
}

/** Utilities to construct instances of class Vector */
object Vector extends VectorConstruction[Vector] {
  def apply(elements:Array[Double]) = new Vector(elements.size, 1, 0, elements)

  /**
   * Actually, the elements are currently initialised to zero
   * but it would be nice to find a way to work that around (TODO).
   */
  def empty(n:Int) = zero(n)
}

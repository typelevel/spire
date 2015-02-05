package spire.algebra

import spire.std._

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.annotation.tailrec
import spire.math.acos

trait Dim3CoordinateSpace[V, @spec(Float, Double) F] extends Any with CoordinateSpace[V, F] {
  override def dimensions : Int = 3
  def cross(v: V, w: V): V 
  def angle(v: V, w: V)(implicit ev: NRoot[F], trig: Trig[F]): F
}

object Dim3CoordinateSpace {
  @inline final def apply[V, @spec(Float,Double) F](implicit V: Dim3CoordinateSpace[V, F]) = V

  def seq[A: Field, CC[A] <: SeqLike[A, CC[A]]](implicit cbf0: CanBuildFrom[CC[A], A, CC[A]]) = 
     new Dim3SeqCoordinateSpace[A, CC[A]]

  def array[@spec(Float, Double) A: Field: ClassTag]: Dim3CoordinateSpace[Array[A], A] =
    new Dim3ArrayCoordinateSpace[A]

}

@SerialVersionUID(0L)
class Dim3SeqCoordinateSpace[A: Field, SA <: SeqLike[A, SA]](implicit cbf: CanBuildFrom[SA,A,SA])
extends SeqInnerProductSpace[A, SA] with Dim3CoordinateSpace[SA, A] with Serializable {
  
  def cross(x: SA, y: SA): SA = {
    val b = cbf()
    b+=(
      // also possible to use _x, _y, _z      
      // x(1) * y(2) - x(2) * y(1)
      scalar.minus(scalar.times(x(1), y(2)), scalar.times(x(2), y(1))),
      // x(2) * y(0) - x(0) * y(2)
      scalar.minus(scalar.times(x(2), y(0)), scalar.times(x(0), y(2))),            
      // x(0) * y(1) - x(1) * y(0)
      scalar.minus(scalar.times(x(0), y(1)), scalar.times(x(1), y(0))))
      
    b.result()    
  } 
  
  def angle(x: SA, y: SA)(implicit nroot: NRoot[A], trig: Trig[A]): A = {
    // law of cosines
    // c2 = x2 + y2 - 2xy cos(angle)
    val x2 = dot(x, x)
    val y2 = dot(y, y)
    val c  = minus(x,y)
    val c2 = dot(c,c)
    val n  = scalar.minus(scalar.plus(x2, y2),c2)
    val d  = scalar.times(scalar.times(NRoot[A].sqrt(x2), NRoot[A].sqrt(y2)), scalar.fromInt(2)) 
    acos(scalar.div(n, d))
  }
  
  def coord(v: SA, i: Int): A = v(i)

  override def dot(v: SA, w: SA): A = super[SeqInnerProductSpace].dot(v, w)

  def axis(i: Int): SA = {
    val b = cbf()

    @tailrec def loop(j: Int): SA = if (i < dimensions) {
      b += (if (i == j) scalar.one else scalar.zero)
      loop(j + 1)
    } else b.result

    loop(0)
  }
  
}

@SerialVersionUID(0L)
class Dim3ArrayCoordinateSpace[@spec(Int,Long,Float,Double) A: ClassTag](implicit val scalar: Field[A])
extends Dim3CoordinateSpace[Array[A], A] with Serializable {
  def cross(x: Array[A], y: Array[A]) : Array[A] =
    Array( 
      // x(1) * y(2) - x(2) * y(1),
      scalar.minus(scalar.times(x(1), y(2)), scalar.times(x(2), y(1))),
      // x(2) * y(0) - x(0) * y(2),
      scalar.minus(scalar.times(x(2), y(0)), scalar.times(x(0), y(2))),            
      // x(0) * y(1) - x(1) * y(0)
      scalar.minus(scalar.times(x(0), y(1)), scalar.times(x(1), y(0))))
  def angle(x: Array[A], y: Array[A])(implicit nroot: NRoot[A], trig: Trig[A]): A = {
    // law of cosines
    // c2 = x2 + y2 - 2xy cos(angle)
    val x2 = dot(x, x)
    val y2 = dot(y, y)
    val c  = minus(x,y)
    val c2 = dot(c,c)
    val n  = scalar.minus(scalar.plus(x2, y2),c2)
    val d  = scalar.times(scalar.times(NRoot[A].sqrt(x2), NRoot[A].sqrt(y2)), scalar.fromInt(2)) 
    acos(scalar.div(n, d))
  }
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
  override def dot(x: Array[A], y: Array[A]): A = ArraySupport.dot(x, y)
  def coord(v: Array[A], i: Int): A = v(i)
  def axis(i: Int): Array[A] = ArraySupport.axis(dimensions, i)
     
}



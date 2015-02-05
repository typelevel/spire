package spire.algebra

import spire.std._

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.annotation.tailrec

trait Dim3CoordinateSpace[V, @spec(Float, Double) F] extends Any with CoordinateSpace[V, F] {

  override def dimensions : Int = 3
  def cross(v: V, w: V): V 
  def :*:(v: V, w: V): V = cross(v,w)
//   def angle(v: V, w: V): F = v dot w
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
      // x(1) * y(2) - x(2) * y(1)
      scalar.plus(scalar.times(x(1), y(2)), scalar.negate(scalar.times(x(2), y(1)))),
      // x(2) * y(0) - x(0) * y(2),
      scalar.plus(scalar.times(x(2), y(0)), scalar.negate(scalar.times(x(0), y(2)))),            
      // x(0) * y(1) - x(1) * y(0)
      scalar.plus(scalar.times(x(0), y(1)), scalar.negate(scalar.times(x(1), y(0)))))
// also possible to use _x, _y, _z      
//      _y(x) * _z(y) - _z(x) * _y(y)
//    scalar.plus(scalar.times(_y(x), _z(y)), scalar.negate(scalar.times(_z(x), _y(y))))
//      _z(x) * _x(y) - _x(x) * _z(y)
//    scalar.plus(scalar.times(_z(x), _x(y)), scalar.negate(scalar.times(_x(x), _z(y))))
//      _x(x) * _y(y) - _y(x) * _x(y)
//    scalar.plus(scalar.times(_x(x), _y(y)), scalar.negate(scalar.times(_y(x), _x(y))))
      
    b.result()    
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
      scalar.plus(scalar.times(x(1), y(2)), scalar.negate(scalar.times(x(2), y(1)))),
      // x(2) * y(0) - x(0) * y(2),
      scalar.plus(scalar.times(x(2), y(0)), scalar.negate(scalar.times(x(0), y(2)))),            
      // x(0) * y(1) - x(1) * y(0)
      scalar.plus(scalar.times(x(0), y(1)), scalar.negate(scalar.times(x(1), y(0)))))
  def zero: Array[A] = new Array[A](0)
  def negate(x: Array[A]): Array[A] = ArraySupport.negate(x)
  def plus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.plus(x, y)
  override def minus(x: Array[A], y: Array[A]): Array[A] = ArraySupport.minus(x, y)
  def timesl(r: A, x: Array[A]): Array[A] = ArraySupport.timesl(r, x)
  override def dot(x: Array[A], y: Array[A]): A = ArraySupport.dot(x, y)
  def coord(v: Array[A], i: Int): A = v(i)
  def axis(i: Int): Array[A] = ArraySupport.axis(dimensions, i)
     
}



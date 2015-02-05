package spire.example

import spire.algebra._
import spire.implicits._
import spire.math.Rational

object Dim3CoordinateSpaceExample extends App {

  implicit val V: Dim3CoordinateSpace[List[Double], Double] = Dim3CoordinateSpace.seq
  implicit val R: Dim3CoordinateSpace[Vector[Rational], Rational] = Dim3CoordinateSpace.seq
  implicit val D: Dim3CoordinateSpace[Array[BigDecimal], BigDecimal] = Dim3CoordinateSpace.array
  //implicit val T: BigDecimalIsTrig = BigDecimalInstances._
  //import spire.std.bigDecimal._
  
  val vd1 = List[Double](1.0, 0, 0)
  val vd2 = List[Double](0, 1.0, 0)
  val vr1 = Vector[Rational](r"1/2", 0, 0)
  val vr2 = Vector[Rational](0, r"1/2", 0)
  val vD1 = Array[BigDecimal](1.0, 0, 0)
  val vD2 = Array[BigDecimal](0, 1.0, 0)
  
  println("Cross product vd1 cross vd2: %s" format (vd1 cross vd2))
  println("Cross product vr1 cross vr2: %s" format (vr1 cross vr2))
  import scala.runtime.ScalaRunTime._
  println("Cross product vD1 cross vD2: %s" format stringOf(vD1 cross vD2))
  
  
  println("vd1 angle vd2: %f rad" format (vd1 angle vd2))
  println("vD1 angle vD2: %f rad" format (vD1 angle vD2))
  // It should be allowed by using object rationalTrig, 
  // but still missing NRoot[Rational] implicit
  //import spire.optional.rationalTrig._
  //println("vr1 angle vr2: %f rad" format (vr1 angle vr2))
}
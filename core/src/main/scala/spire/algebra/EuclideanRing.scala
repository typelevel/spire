package spire.algebra

import spire.macrosk.Ops

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.math.{abs, ceil, floor}

import spire.math.{ConvertableTo, ConvertableFrom, Number}

trait EuclideanRing[@spec(Int,Long,Float,Double) A] extends Ring[A] {
  def quot(a:A, b:A):A
  def mod(a:A, b:A):A
  def quotmod(a:A, b:A): (A, A) = (quot(a, b), mod(a, b))

  //def gcd(a: A, b: A)(implicit eq: Eq[A]): A = euclid(a, b)
  def gcd(a: A, b: A): A

  //def lcm(a: A, b: A)(implicit eq: Eq[A]): A = times(quot(a, gcd(a, b)), b)

  @tailrec protected[this] final def euclid(a:A, b:A)(implicit eq: Eq[A]):A =
    if (eq.eqv(b, zero)) a else euclid(b, mod(a, b))
}

final class EuclideanRingOps[A](lhs:A)(implicit ev:EuclideanRing[A]) {
  def /~(rhs:A) = macro Ops.binop[A, A]
  def %(rhs:A) = macro Ops.binop[A, A]
  def /%(rhs:A) = macro Ops.binop[A, A]

  // TODO: This is a bit
  def /~(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def %(rhs:Int): A = macro Ops.binopWithSelfLift[Int, Ring[A], A]
  def /%(rhs:Int): (A, A) = macro Ops.binopWithSelfLift[Int, Ring[A], (A, A)]

  def /~(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def %(rhs:Double)(implicit ev1:ConvertableTo[A]): A = macro Ops.binopWithLift[Double, ConvertableTo[A], A]
  def /%(rhs:Double)(implicit ev1:ConvertableTo[A]): (A, A) = macro Ops.binopWithLift[Double, ConvertableTo[A], (A, A)]

  def /~(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) /~ rhs
  def %(rhs:Number)(implicit c:ConvertableFrom[A]): Number = c.toNumber(lhs) % rhs
  def /%(rhs:Number)(implicit c:ConvertableFrom[A]): (Number, Number) = c.toNumber(lhs) /% rhs
}

object EuclideanRing {
  @inline final def apply[A](implicit e:EuclideanRing[A]):EuclideanRing[A] = e
}

package spire.algebra

import annotation.tailrec
import scala.{specialized => spec}

import java.lang.Math

import spire.math._
import spire.macrosk.Ops

trait Rng[@spec(Int,Long,Float,Double) A] extends Semiring[A] with AdditiveAbGroup[A]

object Rng {
  implicit def ringIsRng[@spec(Int,Long,Float,Double) A: Ring]: Rng[A] = Ring[A]

  @inline final def apply[A](implicit r:Rng[A]):Rng[A] = r
}

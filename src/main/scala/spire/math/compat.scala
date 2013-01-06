package spire.math

import spire.algebra._

private[math] trait LowViz1 {
  implicit def numeric[A: Ring: ConvertableFrom: Signed: Order]: scala.math.Numeric[A] =
    new ScalaNumericWrapper[A] {
      val order = Order[A]
      val structure = Ring[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }

  implicit def ordering[A: Order]: scala.math.Ordering[A] =
    new ScalaOrderingWrapper[A] {
      val order = Order[A]
    }

  implicit def equiv[A: Eq]: scala.math.Equiv[A] =
    new ScalaEquivWrapper[A] {
      val eq = Eq[A]
    }
}

private[math] trait LowViz2 extends LowViz1 {
  implicit def fractional [A: Field: ConvertableFrom: Signed: Order]: scala.math.Fractional[A] =
    new ScalaFractionalWrapper[A] {
      val order = Order[A]
      val structure = Field[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }
}

private[math] trait LowViz3 extends LowViz2 {
  implicit def integral[A: EuclideanRing: ConvertableFrom: Signed: Order]: scala.math.Integral[A] =
    new ScalaIntegralWrapper[A] {
      val order = Order[A]
      val structure = EuclideanRing[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }
}

object compat extends LowViz3

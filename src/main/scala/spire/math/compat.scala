package spire.math

import spire.algebra._

object compat {
  implicit def equiv[A: Eq] =
    new ScalaEquivWrapper[A] {
      val eq = Eq[A]
    }

  implicit def ordering[A: Order] =
    new ScalaOrderingWrapper[A] {
      val order = Order[A]
    }

  implicit def fractional [A: Field: ConvertableFrom: Signed: Order] =
    new ScalaFractionalWrapper[A] {
      val order = Order[A]
      val structure = Field[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }

  implicit def integral[A: EuclideanRing: ConvertableFrom: Signed: Order] =
    new ScalaIntegralWrapper[A] {
      val order = Order[A]
      val structure = EuclideanRing[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }

  implicit def numeric[A: Ring: ConvertableFrom: Signed: Order] =
    new ScalaNumericWrapper[A] {
      val order = Order[A]
      val structure = Ring[A]
      val conversions = ConvertableFrom[A]
      val signed = Signed[A]
    }
}

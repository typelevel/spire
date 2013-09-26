package spire.matrix

object Transposition extends Enumeration {
  val NoTranspose, Transpose, ConjugateTranspose = Value
}

object UpperOrLower extends Enumeration {
  val Upper, Lower = Value
}

object Sides extends Enumeration {
  val fromLeft, fromRight, Congruent = Value
}
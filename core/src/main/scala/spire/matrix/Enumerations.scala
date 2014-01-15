package spire.matrix

object Transposition extends Enumeration {
  val NoTranspose, Transpose, ConjugateTranspose = Value
}

object UpperOrLower extends Enumeration {
  val Upper, Lower = Value
}

object Sides extends Enumeration {
  val FromLeft, FromRight, Congruent = Value
}

object DiagonalProperty extends Enumeration {
  val UnitDiagonal, NonUnitDiagonal = Value
}

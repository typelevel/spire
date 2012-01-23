package numerics.math

sealed trait Sign {
  def toInt: Int
}
case object Zero extends Sign {
  val toInt = 0
}
case object Positive extends Sign {
  val toInt = 1
}
case object Negative extends Sign {
  val toInt = -1
}

object Sign {
  implicit def sign2int(s: Sign): Int = s.toInt
  implicit def apply(i: Int): Sign =
    if (i == 0) Zero else if (i > 0) Positive else Negative
}



package spire.algebra

/**
 * A simple ADT representing the `Sign` of an object.
 */
sealed abstract class Sign(val toInt: Int) {
  import Sign._

  def unary_-(): Sign = this match {
    case Positive => Negative
    case Negative => Positive
    case Zero => Zero
  }

  def *(that: Sign): Sign = Sign(this.toInt * that.toInt)
}

object Sign {
  case object Zero extends Sign(0)
  case object Positive extends Sign(1)
  case object Negative extends Sign(-1)

  implicit def sign2int(s: Sign): Int = s.toInt
  implicit def apply(i: Int): Sign = 
    if (i == 0) Zero else if (i > 0) Positive else Negative

  implicit object SignAlgebra extends SignAlgebra

  trait SignAlgebra extends Group[Sign] with Signed[Sign] with Order[Sign] {
    def id: Sign = Zero
    def op(a: Sign, b: Sign): Sign = Sign(a.toInt * b.toInt)
    def inverse(a: Sign): Sign = Sign(-a.toInt)
    override def sign(a: Sign): Sign = a
    def signum(a: Sign): Int = a.toInt
    def abs(a: Sign): Sign = if (a == Negative) Positive else a
    def compare(x: Sign, y: Sign): Int = x.toInt - y.toInt
  }
}

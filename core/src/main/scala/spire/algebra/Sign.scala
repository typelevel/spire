package spire.algebra

/**
 * The `Sign` of a number.
 *
 * TODO: `Sign` is a group, isomorphic to {-1,0,1} under multiplication. If we
 *       start using Monoid more seriously (or depend on Scalaz), then Sign
 *       should have a Monoid[Sign] instance.
 */
sealed trait Sign {
  import Sign._

  def toInt: Int

  def unary_-(): Sign = this match {
    case Positive => Negative
    case Negative => Positive
    case Zero => Zero
  }

  def *(that: Sign): Sign = Sign(this.toInt * that.toInt)
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

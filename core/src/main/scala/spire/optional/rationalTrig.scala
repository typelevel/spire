package spire
package optional

import spire.algebra.Trig
import spire.math.Rational

object rationalTrig {
  implicit val trigRational = new Trig[Rational] {
    val r180 = Rational(180)
    import spire.std.double._
    def acos(a: Rational): Rational = Rational(spire.math.acos(a.toDouble))
    def asin(a: Rational): Rational = Rational(spire.math.asin(a.toDouble))
    def atan(a: Rational): Rational = Rational(spire.math.atan(a.toDouble))
    def atan2(y: Rational, x: Rational): Rational = Rational(spire.math.atan2(y.toDouble, x.toDouble))
    def cos(a: Rational): Rational = Rational(spire.math.cos(a.toDouble))
    def cosh(x: Rational): Rational = Rational(spire.math.cosh(x.toDouble))
    val e: Rational = Rational(spire.math.e)
    def exp(a: Rational): Rational = Rational(spire.math.exp(a.toDouble))
    def expm1(a: Rational): Rational = Rational(spire.math.expm1(a.toDouble))
    def log(a: Rational): Rational = Rational(spire.math.log(a.toDouble))
    def log1p(a: Rational): Rational = Rational(spire.math.log1p(a.toDouble))
    val pi: Rational = Rational(spire.math.pi)
    def sin(a: Rational): Rational = Rational(spire.math.sin(a.toDouble))
    def sinh(x: Rational): Rational = Rational(spire.math.sinh(x.toDouble))
    def tan(a: Rational): Rational = Rational(spire.math.tan(a.toDouble))
    def tanh(x: Rational): Rational = Rational(spire.math.tanh(x.toDouble))
    def toDegrees(a: Rational): Rational = (a * r180) / pi
    def toRadians(a: Rational): Rational = (a / r180) * pi
  }
}

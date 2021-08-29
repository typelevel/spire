package spire
package random

import spire.algebra._
import spire.std.float._
import spire.std.double._
import spire.std.bigDecimal._

class GaussianSuite extends munit.FunSuite {
  import AndersonDarlingTest._

  def checkGaussian[A: Field: Trig: NRoot: IsReal: ClassTag](nextGaussian: (A, A) => A): Unit = {
    val mean = Field[A].zero
    val stdDev = Field[A].one
    val xs = Array.fill(20)(nextGaussian(mean, stdDev))
    assert(isGaussian(xs, mean, stdDev))
  }

  test("rng.Lcg64#nextGaussian is normal") {
    val gen = rng.Lcg64.fromTime(42L)
    checkGaussian[Double](gen.nextGaussian(_, _))
  }

  def checkMarsagliaGaussian[A: Field: NRoot: Trig: IsReal: Uniform: ClassTag] = {
    val gen = rng.Cmwc5.fromTime(42L)
    val gaussian = new MarsagliaGaussian[A]
    checkGaussian[A] { (mean, stdDev) =>
      gaussian(mean, stdDev)(gen)
    }
  }

  test("MarsagliaGaussian[Float] is normal")(checkMarsagliaGaussian[Float])
  test("MarsagliaGaussian[Double] is normal")(checkMarsagliaGaussian[Double])
  // test("MarsagliaGaussian[BigDecimal] is normal")(checkMarsagliaGaussian[BigDecimal])
}

object AndersonDarlingTest {
  import spire.syntax.field._
  import spire.syntax.nroot._
  import spire.syntax.trig._
  import spire.syntax.isReal._
  import spire.syntax.std.array._

  // Anderson-Darling test.
  def isGaussian[A: Field: Trig: NRoot: IsReal: ClassTag](xs: Array[A], mean: A, stdDev: A): Boolean = {
    @tailrec def loop(sum: A, i: Int, a: A, b: A): A = if (i < xs.length) {
      // val y = cdf((xs(i) - mean) / stdDev, mean, stdDev)
      val y = cdf(xs(i), mean, stdDev)
      val k = a * y.log() + b * (1 - y).log()
      loop(sum + k, i + 1, a + 2, b - 2)
    } else sum

    xs.qsort
    val n = Field[A].fromInt(xs.length)
    val sum = loop(Field[A].zero, 0, Field[A].one, 2 * n - 1)
    val score = -n - sum / n
    score < Field[A].fromDouble(2.492) // 5% significance.
  }

  def cdf[A: Field: Trig: NRoot: IsReal](x: A, mean: A, stdDev: A): A =
    0.5 * erfc((mean - x) / (Field[A].fromInt(2).sqrt() * stdDev))

  // Approximation from: http://en.wikipedia.org/wiki/Error_function#Approximation_with_elementary_functions
  // which listed Abramowitz and Stegun as the source.
  def erfc[A: Field: Trig: IsReal](x: A): A = if (x.signum() < 0) {
    2 - erfc(-x)
  } else {
    val t = 1 / (1 + 0.3275911 * x)
    val y = t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429))))
    (-x * x).exp() * y
  }
}

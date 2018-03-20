package spire.stats.test

import spire.std.double._

import org.scalatest.Matchers
import org.scalatest.WordSpec

class AndersonDarlingSpec extends WordSpec with Matchers {

  "The Anderson–Darling test" should {
    "calculate a statistic that quantifies if the values were drawn from a standard uniform distribution" in {
      val dist = spire.stats.distribution.Uniform.Standard[Double]
      val n = 5
      val ys = Array(0.3, 0.1, 0.9, 0.5, 0.7)
      val lncdfs = Array(-2.302585093, -1.203972804, -0.693147181, -0.356674944, -0.105360516)
      val ln1cdfs = lncdfs.reverse

      val expected = -n - (1.0 / n) * (
        1 * lncdfs(0) + 9 * ln1cdfs(0) +
        3 * lncdfs(1) + 7 * ln1cdfs(1) +
        5 * lncdfs(2) + 5 * ln1cdfs(2) +
        7 * lncdfs(3) + 3 * ln1cdfs(3) +
        9 * lncdfs(4) + 1 * ln1cdfs(4)
      )

      val (result, _, _) = AndersonDarling(ys, dist)

      result should equal (expected +- 1e-8)
    }

    "calculate a statistic that quantifies if the values were drawn from a normal distribution" in {
      // from: D'Agostino, Stephens. (1986). "Goodness-of-fit-techniques."
      val chickData: Array[Double] = Array(
        156,162,168,182,186,
        190,190,196,202,210,
        214,220,226,230,230,
        236,236,242,246,270
      )

      val dist = spire.stats.distribution.Normal(200.0, 35.0)

      val expected = 1.0169

      val (result, _, _) = AndersonDarling(chickData, dist)

      result should equal (expected +- 1e-4)
    }
  }
}


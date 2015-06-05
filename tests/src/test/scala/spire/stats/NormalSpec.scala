package spire.stats.distribution

import spire.std.double._

import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class NormalSpec extends WordSpec with Matchers with TableDrivenPropertyChecks {

  "A standard normal distribution (mean == 0 and stdDev == 1)" should {
    val standard = Normal.Standard[Double]

    "have pdf equals to phi(x)" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          (-3.0, 0.00443),
          (-2.0, 0.05399),
          (-1.0, 0.24197),
          ( 0.0, 0.39894),
          ( 1.0, 0.24197),
          ( 2.0, 0.05399),
          ( 3.0, 0.00443)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        standard.pdf(x) should equal (pdf +- 1e-5)
      }
    }

    "have cdf equals to Phi(x)" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          (-3.0, 0.00135),
          (-2.0, 0.02275),
          (-1.0, 0.15866),
          ( 0.0, 0.50000),
          ( 1.0, 0.84134),
          ( 2.0, 0.97725),
          ( 3.0, 0.99865)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf +- 1e-5)
      }
    }
  }

  "A normal distribution with mean == 1 and stdDev == 2" should {
    val general = Normal[Double](1.0, 2.0)

    info("for z = (x - mean) / stdDev")
    "have pdf equals to phi(z) / stdDev" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          (-5.0, 0.00222),
          (-3.0, 0.02700),
          (-1.0, 0.12099),
          ( 1.0, 0.19947),
          ( 3.0, 0.12099),
          ( 5.0, 0.02700),
          ( 7.0, 0.00222)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        general.pdf(x) should equal (pdf +- 1e-5)
      }
    }

    "have cdf equals to Phi(z)" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          (-5.0, 0.00135),
          (-3.0, 0.02275),
          (-1.0, 0.15866),
          ( 1.0, 0.50000),
          ( 3.0, 0.84134),
          ( 5.0, 0.97725),
          ( 7.0, 0.99865)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        general.cdf(x) should equal (cdf +- 1e-5)
      }
    }
  }
}


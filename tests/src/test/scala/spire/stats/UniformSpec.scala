package spire.stats.distribution

import spire.std.double._

import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class UniformSpec extends WordSpec with Matchers with TableDrivenPropertyChecks {

  "A standard uniform distribution (min == 0 and max == 1)" should {
    val standard = Uniform.Standard[Double]

    "have pdf equals to 1 for 0 <= x <= 1" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          ( 0.0, 1.0),
          ( 0.5, 1.0),
          ( 1.0, 1.0)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        standard.pdf(x) should equal (pdf)
      }
    }

    "have pdf equals to 0 for x < 0 or 1 < x" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          (-0.5, 0.0),
          ( 1.5, 0.0)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        standard.pdf(x) should equal (pdf)
      }
    }

    "have cdf equals to 0 for x <= 0" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          (-0.5, 0.0),
          ( 0.0, 0.0)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf)
      }
    }

    "have cdf equals to x for 0 <= x <= 1" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          ( 0.5, 0.5)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf)
      }
    }

    "have cdf equals to 1 for 1 <= x" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          ( 1.0, 1.0),
          ( 1.5, 1.0)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf)
      }
    }
  }

  "An uniform distribution with min == 1 and max == 3" should {
    val standard = Uniform[Double](1.0, 3.0)

    "have pdf equals to (1 / (max - min)) for min <= x <= max" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          ( 1.0, 0.5),
          ( 2.0, 0.5),
          ( 3.0, 0.5)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        standard.pdf(x) should equal (pdf)
      }
    }

    "have pdf equals to 0 for x < min or max < x" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          ( 0.5, 0.0),
          ( 3.5, 0.0)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        standard.pdf(x) should equal (pdf)
      }
    }

    "have cdf equals to 0 for x <= min" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          ( 0.5, 0.0),
          ( 1.0, 0.0)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf)
      }
    }

    "have cdf equals to (x - a) / (b - a) for min <= x <= max" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          ( 2.0, 0.5)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf)
      }
    }

    "have cdf equals to 1 for max <= x" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          ( 3.0, 1.0),
          ( 3.5, 1.0)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf)
      }
    }
  }
}


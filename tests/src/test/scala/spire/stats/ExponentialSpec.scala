package spire.stats.distribution

import spire.std.double._

import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class ExponentialSpec extends WordSpec with Matchers with TableDrivenPropertyChecks {

  "A standard exponential distribution (mode == 0 and rate == 1)" should {
    val standard = Exponential.Standard[Double]

    "have pdf equals to exp(-x) for x >= 0" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          ( 0.0, 1.000000000),
          ( 1.0, 0.367879441),
          ( 2.0, 0.135335283),
          ( 3.0, 0.049787068)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        standard.pdf(x) should equal (pdf +- 1e-5)
      }
    }

    "have pdf equals to 0 for x < 0" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          (-3.0, 0.0),
          (-2.0, 0.0),
          (-1.0, 0.0)
        )
      forAll(pdfs) { (x: Double, pdf: Double) =>
        standard.pdf(x) should equal (pdf +- 1e-5)
      }
    }

    "have cdf equals to (1 - exp(-x)) for x >= 0" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          ( 0.0, 0.000000000),
          ( 1.0, 0.632120559),
          ( 2.0, 0.864664717),
          ( 3.0, 0.950212932)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf +- 1e-5)
      }
    }

    "have cdf equals to 0 for x < 0" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          (-3.0, 0.0),
          (-2.0, 0.0),
          (-1.0, 0.0)
        )
      forAll(cdfs) { (x: Double, cdf: Double) =>
        standard.cdf(x) should equal (cdf +- 1e-5)
      }
    }
  }

  "An exponential distribution with mode == 1 and rate == 0.5" should {
    val general = Exponential[Double](1.0, 0.5)

    info("for z = (x - mode) * rate")
    "have pdf equals to exp(-z) for z >= 0" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          ( 1.0, 0.500000000),
          ( 3.0, 0.183939720),
          ( 5.0, 0.067667641),
          ( 7.0, 0.024893534)
        )
      forAll(pdfs) { (z: Double, pdf: Double) =>
        general.pdf(z) should equal (pdf +- 1e-5)
      }
    }

    "have pdf equals to 0 for z < 0" in {
      val pdfs =
        Table(
          ("x", "pdf"),
          (-5.0, 0.0),
          (-3.0, 0.0),
          (-1.0, 0.0)
        )
      forAll(pdfs) { (z: Double, pdf: Double) =>
        general.pdf(z) should equal (pdf +- 1e-5)
      }
    }

    "have cdf equals to (1 - exp(-z)) for z >= 0" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          ( 1.0, 0.000000000),
          ( 3.0, 0.632120559),
          ( 5.0, 0.864664717),
          ( 7.0, 0.950212932)
        )
      forAll(cdfs) { (z: Double, cdf: Double) =>
        general.cdf(z) should equal (cdf +- 1e-5)
      }
    }

    "have cdf equals to 0 for z < 0" in {
      val cdfs =
        Table(
          ("x", "cdf"),
          (-5.0, 0.0),
          (-3.0, 0.0),
          (-1.0, 0.0)
        )
      forAll(cdfs) { (z: Double, cdf: Double) =>
        general.cdf(z) should equal (cdf +- 1e-5)
      }
    }
  }
}


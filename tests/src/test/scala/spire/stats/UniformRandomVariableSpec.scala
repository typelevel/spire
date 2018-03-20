package spire.stats

import spire.std.double._

import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class UniformRandomVariableSpec extends WordSpec with Matchers {

  val srv = RandomVariable(distribution.Uniform.Standard[Double])

  "A random variable with standard uniform distribution" should {
    "observe values equal to underlying generator's values" in {
      val gen1 = spire.random.rng.Lcg64.fromTime(42L)
      val gen2 = spire.random.rng.Lcg64.fromTime(42L)

      val result = srv.observeMany(gen1, 20)
      val expected = Array.fill(20)(gen2.nextDouble)

      result should equal (expected)
    }
  }

  "A random variable with uniform distribution" should {
    val dist = distribution.Uniform[Double](1.0, 3.0)
    val grv = RandomVariable(dist)

    info("as a member of a location-scale family")
    "observe values equal to standard distribution's values scaled and shifted" in {
      val gen1 = spire.random.rng.Lcg64.fromTime(42L)
      val gen2 = spire.random.rng.Lcg64.fromTime(42L)

      val result = grv.observeMany(gen1, 20)
      val expected = srv.observeMany(gen2, 20).map(_ * 2 + 1)

      result should equal (expected)
    }

    "observe values according to its probability distribution function" in {
      val gen = spire.random.rng.Lcg64.fromTime(42L)
      val ys = grv.observeMany(gen, 20)

      val rejected = test.AndersonDarling.rejectedAt(ys, dist, 0.05)

      rejected should equal (false)
    }
  }
}


package spire.random

import scala.{specialized => spec}

import spire.math._

trait Uniform[@spec A] { self =>
  /**
   * Return an `A` that is uniformly distributed between `min` and `max` inclusive.
   */
  def apply(min: A, max: A): Dist[A]
}

object Uniform {
  @inline final def apply[@spec A](implicit u: Uniform[A]): Uniform[A] = u

  implicit def UniformInt = new Uniform[Int] {
    def apply(min: Int, max: Int): Dist[Int] =
      new DistFromGen[Int](_.nextInt(min, max))
  }

  implicit def UniformLong = new Uniform[Long] {
    def apply(min: Long, max: Long): Dist[Long] =
      new DistFromGen[Long](_.nextLong(min, max))
  }

  implicit def UniformUInt = new Uniform[UInt] {
    def apply(min: UInt, max: UInt): Dist[UInt] =
      new DistFromGen[UInt](g => UInt(g.nextInt(min.signed, max.signed)))
  }

  implicit def UniformULong = new Uniform[ULong] {
    def apply(min: ULong, max: ULong): Dist[ULong] =
      new DistFromGen[ULong](g => ULong(g.nextLong(min.signed, max.signed)))
  }

  implicit def UniformFloat = new Uniform[Float] {
    def apply(min: Float, max: Float): Dist[Float] =
      new DistFromGen[Float](_.nextFloat(min, max))
  }

  implicit def UniformDouble = new Uniform[Double] {
    def apply(min: Double, max: Double): Dist[Double] =
      new DistFromGen[Double](_.nextDouble(min, max))
  }

  implicit def UniformBigInt = new Uniform[BigInt] {
    def apply(min: BigInt, max: BigInt): Dist[BigInt] = {
      val range = max - min
      val width = range.bitLength
      if (width < 64) {
        val range0 = range.toLong
        new DistFromGen[BigInt]({ gen =>
          min + BigInt(gen.nextLong(0, range0))
        })
      } else {
        val mask0 = (1 << (width % 8)) - 1
        val mask = if (mask0 == 0) 255 else mask0
        new DistFromGen[BigInt]({ gen =>
          val bytes = new Array[Byte]((width + 7) / 8)
          
          def loop(): BigInt = {
            gen.fillBytes(bytes)
            bytes(0) = (bytes(0) & mask).toByte
            val n = BigInt(1, bytes)
            if (n > range) loop() else n
          }

          min + loop()
        })
      }
    }
  }

  implicit def UniformBigDecimal = new Uniform[BigDecimal] {
    def apply(min: BigDecimal, max: BigDecimal): Dist[BigDecimal] = {
      val precision = spire.math.max(min.mc.getPrecision, max.mc.getPrecision)
      if (precision == 0) {
        throw new IllegalArgumentException("Both min and max provided to UniformBigDecimal have unlimited precision. Cannot produce uniform distributions with unlimited precision.")
      }
      val range = max - min
      val dist = UniformBigInt(0, BigInt(10) pow precision)
      new DistFromGen[BigDecimal]({ gen =>
        min + range * BigDecimal(dist(gen), precision)
      })
    }
  }

  def uniformRational(eps: Rational) = new Uniform[Rational] {
    def apply(min: Rational, max: Rational): Dist[Rational] = {
      val num = ((max - min) / eps).toBigInt
      Uniform[BigInt].apply(0, num).map(n => min + Rational(n) * eps)
    }
  }

}

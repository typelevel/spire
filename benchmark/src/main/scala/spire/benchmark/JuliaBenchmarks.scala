package spire
package benchmark
/*
import spire.implicits._
import spire.math._

object JuliaBenchmarks extends MyRunner(classOf[JuliaBenchmarks])

class JuliaBenchmarks extends MyBenchmark {
  def mandelComplex(c: Complex[Double]): Int = {
    var z = c
    var n = 1
    while (n < 80) {
      if (z.abs > 2.0) return n - 1
      z = z * z + c
      n += 1
    }
    80
  }

  def timeMandelComplex(reps:Int) = run(reps) {
    var total = 0
    var r = -2.0
    var i = 0
    while (i < 25) {
      var c = -1.0
      var j = 0
      while (j < 20) {
        total += mandelComplex(Complex(r, c))
        c = c + 0.1
        j += 1
      }
      r = r + 0.1
      i += 1
    }
    total
  }

  def mandelFloat(c: FloatComplex): Int = {
    var z = c
    var n = 1
    while (n < 80) {
      if (z.abs > 2.0F) return n - 1
      z = z * z + c
      n += 1
    }
    80
  }

  def timeMandelFloat(reps:Int) = run(reps) {
    var total = 0
    var r = -2.0
    var i = 0
    while (i < 25) {
      var c = -1.0
      var j = 0
      while (j < 20) {
        total += mandelFloat(FloatComplex(r.toFloat, c.toFloat))
        c = c + 0.1
        j += 1
      }
      r = r + 0.1
      i += 1
    }
    total
  }

  def mandelFast(c: Long): Int = {
    var z = c
    var n = 1
    while (n < 80) {
      if (FastComplex.abs(z) > 2.0F) return n - 1
      z = FastComplex.add(FastComplex.multiply(z, z), c)
      n += 1
    }
    80
  }

  def timeMandelFast(reps: Int) = run(reps) {
    var total = 0
    var r = -2.0
    var i = 0
    while (i < 25) {
      var c = -1.0
      var j = 0
      while (j < 20) {
        total += mandelFast(FastComplex(r.toFloat, c.toFloat))
        c = c + 0.1
        j += 1
      }
      r = r + 0.1
      i += 1
    }
    total
  }
}
*/
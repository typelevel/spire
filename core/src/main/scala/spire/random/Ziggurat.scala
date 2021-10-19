/**
 * **********************************************************************\ * Project ** * ______ ______ __ ______ ____
 * ** * / ____/ / __ / / / / __ / / __/ (c) 2011-2014 ** * / /__ / /_/ / / / / /_/ / / /_ ** * /___ / / ____/ / / / __ /
 * / __/ Erik Osheim, Tom Switzer ** * ____/ / / / / / / / | | / /__ ** * /_____/ /_/ /_/ /_/ |_| /____/ All rights
 * reserved. ** * ** * Redistribution and use permitted under the MIT license. ** * **
 * \***********************************************************************
 */

package spire
package random

/**
 * This is a Scala implementation of the Ziggurat algorithm for generating random variables from decreasing densities.
 *
 * <p><b>Reference: </b> George Marsaglia, Wai Wan Tsang: "The Ziggurat Method for Generating Random Variables"
 * <i>Journal of Statistical Software,</i> Vol. 5, Issue 8, October 2000.
 *
 * @see
 *   <a href="http://www.jstatsoft.org/v05/i08">Ziggurat Paper</a>
 * @see
 *   <a href="http://en.wikipedia.org/wiki/Ziggurat_algorithm">Ziggurat algorithm @ Wikipedia</a>
 * @author
 *   <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */
object Ziggurat {

  import scala.math.exp
  import scala.math.sqrt
  import scala.math.log
  import scala.math.abs

  private val kn = new Array[Long](128)
  private val wn = new Array[Double](128)
  private val fn = new Array[Double](128)

  private val ke = new Array[Long](256)
  private val we = new Array[Double](256)
  private val fe = new Array[Double](256)

  def rnor(g: Generator): Double = {

    val hz = g.nextInt()
    val iz = hz & 127

    if (abs(hz) < kn(iz)) hz * wn(iz) else nfix(g, hz, iz)
  }

  def rexp(g: Generator): Double = {

    val jz = g.nextInt() & 0xffffffffL
    val iz = (jz & 255).toInt

    if (jz < ke(iz)) jz * we(iz) else efix(g, jz, iz)
  }

  private def nfix(g: Generator, hza: Int, iza: Int): Double = {

    val r = 3.442619855899d
    val r1 = 1 / r
    var x, y = 0d
    var hz = hza
    var iz = iza

    @tailrec def loop: Double = {
      x = hz * wn(iz)

      if (iz == 0) {
        while ({
          x = -log(g.nextDouble()) * r1
          y = -log(g.nextDouble())
          (y + y < x * x)
        })
          return if (hz > 0) r + x else -r - x
      }

      if (fn(iz) + g.nextDouble() * (fn(iz - 1) - fn(iz)) < exp(-.5 * x * x)) return x

      hz = g.nextInt()
      iz = hz & 127
      if (abs(hz) < kn(iz)) return hz * wn(iz)

      loop
    }

    loop
  }

  private def efix(g: Generator, jza: Long, iza: Int): Double = {

    var jz = jza
    var iz = iza

    @tailrec def loop: Double = {
      if (iz == 0) return 7.697117470131487 - log(g.nextDouble())
      val x = jz * we(iz)

      if (fe(iz) + g.nextDouble() * (fe(iz - 1) - fe(iz)) < exp(-x)) return x

      jz = g.nextInt() & 0xffffffffL
      iz = (jz & 255).toInt
      if (jz < ke(iz)) return jz * we(iz)

      loop
    }

    loop
  }

  {
    val m1: Double = 2147483648d
    val m2: Double = 4294967296d

    var dn: Double = 3.442619855899
    var tn: Double = dn
    var de: Double = 7.697117470131487
    var te: Double = de

    val vn: Double = 9.91256303526217e-3
    val ve: Double = 3.949659822581572e-3

    var q: Double = vn / exp(-.5 * dn * dn)
    kn(0) = ((dn / q) * m1).toLong
    kn(1) = 0

    wn(0) = q / m1
    wn(127) = dn / m1

    fn(0) = 1d
    fn(127) = exp(-.5 * dn * dn)

    for (i <- 126 to 1 by -1) {
      dn = sqrt(-2 * log(vn / dn + exp(-.5 * dn * dn)))
      kn(i + 1) = ((dn / tn) * m1).toLong
      tn = dn
      fn(i) = exp(-.5 * dn * dn)
      wn(i) = dn / m1
    }

    q = ve / exp(-de)
    ke(0) = ((de / q) * m2).toLong
    ke(1) = 0

    we(0) = q / m2
    we(255) = de / m2

    fe(0) = 1d
    fe(255) = exp(-de)

    for (i <- 254 to 1 by -1) {
      de = -log(ve / de + exp(-de))
      ke(i + 1) = ((de / te) * m2).toLong
      te = de
      fe(i) = exp(-de)
      we(i) = de / m2
    }
  }
}

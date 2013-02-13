package spire.random

import spire.math._

final class CmwcPrng(_x: Long, _y: Long, _z: Long, _w: Long, _v: Long) extends LongGenerator {
  private var x: Long = _x
  private var y: Long = _y
  private var z: Long = _z
  private var w: Long = _w
  private var v: Long = _v

  def copy: Generator = new CmwcPrng(x, y, z, w, v)

  def nextLong(): Long = {
    val t: Long = x ^ (x >>> 7)
    x = y
    y = z
    z = w
    w = v
    v = (v ^ (v << 6)) ^ (t ^ (t << 13))
    (y + y + 1) * v
  }
}

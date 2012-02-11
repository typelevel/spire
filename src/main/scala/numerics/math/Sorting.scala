package numerics.math

import scala.{specialized => spec}
import scala.math.min

import Implicits._

object Sorting {
  private final def merge[@spec A:Order](in:Array[A], out:Array[A],
                                         start:Int, mid:Int, end:Int) {
    var i = start
    var j = mid
    var k = start
    while (k < end) {
      if (i < mid && (j >= end || Order[A].lteq(in(i), in(j)))) {
        out(k) = in(i); i += 1
      } else {
        out(k) = in(j); j += 1
      }
      k += 1
    }
  }
  
  final def sort[@spec A:Order:Manifest](data:Array[A]) = {
    val len = data.length

    var buf1:Array[A] = data.clone()
    var buf2:Array[A] = Array.ofDim[A](len)
    var tmp:Array[A] = null

    var i = 0

    var width = 1
    var step = 2
    while (width < len) {
      i = 0
      while (i < len) {
        merge(buf1, buf2, i, min(i + width, len), min(i + step, len))
        i += step
      }
      tmp = buf2
      buf2 = buf1
      buf1 = tmp

      width *= 2
      step *= 2
    }

    buf1
  }

  final def sortInPlace[@spec A:Order:Manifest](data:Array[A]) {

    @inline def recurse(data:Array[A], len:Int, start:Int, end:Int) = {
      val subdata = Array.ofDim[A](len)
      Array.copy(data, start, subdata, 0, end)
      if (len > 1) sortInPlace(subdata)
      subdata
    }

    val len = data.length
    val llen = len / 2
    val rlen = len - llen

    val left = recurse(data, llen, 0, llen)
    val right = recurse(data, rlen, llen, rlen)

    var i = 0
    var j = 0
    var k = 0

    while (k < len) {
      if (i < llen && (j == rlen || Order[A].lteq(left(i), right(j)))) {
        data(k) = left(i); i += 1
      } else {
        data(k) = right(j); j += 1
      }
      k += 1
    }
  }

  def mySort[@spec K:Order](a:Array[K]) { sort1(a, 0, a.length) }

  private def sort1[@spec K](x:Array[K], off:Int, len:Int)(implicit ev:Order[K]) {
    def swap(k:K, a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
    def vecswap(k:K, _a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(k, a, b)
        i += 1
        a += 1
        b += 1
      }
    }
    def med3(k:K, a: Int, b: Int, c: Int) = {
      if (ev.lt(x(a), x(b))) {
        if (ev.lt(x(b), x(c))) b else if (ev.lt(x(a), x(c))) c else a
      } else {
        if (ev.lt(x(b), x(c))) b else if (ev.lt(x(a), x(c))) c else a
      }
    }
    def sort2(k:K, off: Int, len: Int) {
      // Insertion sort on smallest arrays
      if (len < 7) {
        var i = off
        while (i < len + off) {
          var j = i
          while (j > off && ev.gt(x(j-1), x(j))) {
            swap(k, j, j-1)
            j -= 1
          }
          i += 1
        }
      } else {
        // Choose a partition element, v
        var m = off + (len >> 1)        // Small arrays, middle element
        if (len > 7) {
          var l = off
          var n = off + len - 1
          if (len > 40) {        // Big arrays, pseudomedian of 9
            val s = len / 8
            l = med3(k, l, l+s, l+2*s)
            m = med3(k, m-s, m, m+s)
            n = med3(k, n-2*s, n-s, n)
          }
          m = med3(k, l, m, n) // Mid-size, med of 3
        }
        val v = x(m)

        // Establish Invariant: v* (<v)* (>v)* v*
        var a = off
        var b = a
        var c = off + len - 1
        var d = c
        var done = false
        while (!done) {
          while (b <= c && ev.lteq(x(b), v)) {
            if (ev.eq(x(b), v)) {
              swap(k, a, b)
              a += 1
            }
            b += 1
          }
          while (c >= b && ev.gteq(x(c), v)) {
            if (ev.eq(x(c), v)) {
              swap(k, c, d)
              d -= 1
            }
            c -= 1
          }
          if (b > c) {
            done = true
          } else {
            swap(k, b, c)
            c -= 1
            b += 1
          }
        }

        // Swap partition elements back to middle
        val n = off + len
        var s = math.min(a-off, b-a)
        vecswap(k, off, b-s, s)
        s = math.min(d-c, n-d-1)
        vecswap(k, b, n-s, s)

        // Recursively sort non-partition-elements
        s = b - a
        if (s > 1)
          sort2(k, off, s)
        s = d - c
        if (s > 1)
          sort2(k, n-s, s)
      }
    }
    sort2(x(0), off, len)
  }

}

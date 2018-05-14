package spire.math.cf

import scala.annotation.tailrec

import spire.math.{Extended, CF}
import CF.{Term, Infinity}

object Eval {

  import spire.math.{SafeLong => Z}

  def eval4(a: Z, b: Z, c: Z, d: Z, cf: CF): CF = {
    @tailrec def loop4(a: Z, b: Z, c: Z, d: Z, cf: CF): CF = {
      if (c == 0 && d == 0) return Infinity

      if (c != 0 && d != 0) {
        val q = a / c
        if (q == b / d) return q ~: eval4(c, d, a - c * q, b - d * q, cf)
      }

      cf match {
        case Term(n, f) => loop4(a * n + b, a, c * n + d, c, f())
        case inf => loop4(a, a, c, c, inf)
      }
    }
    loop4(a, b, c, d, cf)
  }

  def eval8(
    breakout: Int, doneLeft: Boolean, doneRight: Boolean, lhs: CF, rhs: CF,
    a: Z, b: Z, c: Z, d: Z, e: Z, f: Z, g: Z, h: Z): CF = {

    @tailrec def loop8(
      i: Int, doneLeft: Boolean, doneRight: Boolean, lhs: CF, rhs: CF,
      a: Z, b: Z, c: Z, d: Z, e: Z, f: Z, g: Z, h: Z): CF = {

      // FIXME: breakout is a hack to support approximately results
      // that are exactly zero.
      if (i >= breakout || (e == 0 && f == 0 && g == 0 && h == 0)) return Infinity

      val ae = Extended(a) /~ Extended(e)
      val bf = Extended(b) /~ Extended(f)
      val cg = Extended(c) /~ Extended(g)
      val dh = Extended(d) /~ Extended(h)

      if (ae === bf && bf === cg && cg == dh) {
        val q = ae.getOrError()
        return q ~: eval8(
          breakout, doneLeft, doneRight, lhs, rhs,
          e, f, g, h,
          a - e * q, b - f * q, c - g * q, d - h * q)
      }

      def diff(x: Extended[Z], y: Extended[Z]): Extended[Z] =
        (x - y).abs.undefToZero

      val xw = diff(ae, cg) max diff(bf, dh)
      val yw = diff(ae, bf) max diff(cg, dh)
      val takeLeft = doneRight || (!doneLeft && xw > yw)

      if (takeLeft) {
        lhs match {
          case Term(n, ff) =>
            loop8(i + 1, doneLeft, doneRight, ff(), rhs,
              a * n + c, b * n + d, a, b,
              e * n + g, f * n + h, e, f)
          case inf =>
            loop8(i + 1, true, doneRight, inf, rhs,
              a, b, a, b,
              e, f, e, f)
        }
      } else {
        rhs match {
          case Term(n, ff) =>
            loop8(i + 1, doneLeft, doneRight, lhs, ff(),
              a * n + b, a, c * n + d, c,
              e * n + f, e, g * n + h, g)
          case inf =>
            loop8(i + 1, doneLeft, true, lhs, inf,
              a, a, c, c,
              e, e, g, g)
        }
      }
    }
    loop8(0, doneLeft, doneRight, lhs, rhs, a, b, c, d, e, f, g, h)
  }
}

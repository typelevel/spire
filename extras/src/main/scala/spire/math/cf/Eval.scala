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
      i: Int, doneLeft: Boolean, doneRight: Boolean, tookLeft: Boolean, lhs: CF, rhs: CF,
      a: Z, b: Z, c: Z, d: Z, e: Z, f: Z, g: Z, h: Z): CF = {

      // FIXME: breakout is a hack to support approximately results
      // that are exactly zero.
      if (i >= breakout || (e == 0 && f == 0 && g == 0 && h == 0)) return Infinity

      def bnd(n: Z, d: Z): Extended[Z] =
        if (d.isZero) Extended.inf(1) else Extended(n / d)

      val (i11, i01, i10, i00) = (bnd(a, e), bnd(c, g), bnd(b, f), bnd(d, h))

      if (i11 === i01 && i01 === i10 && i10 == i00) {
        val q = i00.getOrError()
        return q ~: eval8(
          breakout, doneLeft, doneRight, lhs, rhs,
          e, f, g, h,
          a - e * q, b - f * q, c - g * q, d - h * q)
      }

      def diff(x: Extended[Z], y: Extended[Z]): Extended[Z] =
        if (x.isInfinite) x
        else if (y.isInfinite) y
        else Extended.Finite(x.getOrError - y.getOrError)

      val xw = diff(i11, i01) max diff(i10, i00)
      val yw = diff(i11, i10) max diff(i01, i00)
      val takeLeft = doneRight || xw > yw || (xw == yw && !tookLeft)

      if (takeLeft) {
        lhs match {
          case CF.Term(n, ff) =>
            loop8(i + 1, doneLeft, doneRight, true, ff(), rhs,
              a * n + c, b * n + d, a, b,
              e * n + g, f * n + h, e, f)
          case inf @ CF.Infinity =>
            loop8(i + 1, true, doneRight, true, inf, rhs,
              a, b, a, b,
              e, f, e, f)
        }
      } else {
        rhs match {
          case CF.Term(n, ff) =>
            loop8(i + 1, doneLeft, doneRight, false, lhs, ff(),
              a * n + b, a, c * n + d, c,
              e * n + f, e, g * n + h, g)
          case inf @ CF.Infinity =>
            loop8(i + 1, doneLeft, true, false, lhs, inf,
              a, a, c, c,
              e, e, g, g)
        }
      }
    }
    loop8(0, doneLeft, doneRight, false, lhs, rhs, a, b, c, d, e, f, g, h)
  }
}

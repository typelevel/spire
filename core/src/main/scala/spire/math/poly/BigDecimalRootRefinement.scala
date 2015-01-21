package spire.math
package poly

import scala.annotation.tailrec

import java.math.{ BigDecimal => JBigDecimal, RoundingMode, MathContext }

import spire.algebra._
import spire.syntax.signed._

sealed abstract class BigDecimalRootRefinement {
  def approximation: JBigDecimal
}

object BigDecimalRootRefinement {
  case class ExactRoot(root: JBigDecimal) extends BigDecimalRootRefinement {
    def approximation: JBigDecimal = root
  }

  case class Bounded(lowerBound: JBigDecimal, upperBound: JBigDecimal) extends BigDecimalRootRefinement {
    def approximation: JBigDecimal = lowerBound
  }

  case class BoundedLeft(lowerBound: Rational, upperBound: JBigDecimal) extends BigDecimalRootRefinement {
    def approximation: JBigDecimal = upperBound
  }

  case class BoundedRight(lowerBound: JBigDecimal, upperBound: Rational) extends BigDecimalRootRefinement {
    def approximation: JBigDecimal = lowerBound
  }

  private implicit object JBigDecimalOrder extends Signed[JBigDecimal] with Order[JBigDecimal] {
    def signum(a: JBigDecimal): Int = a.signum
    def abs(a: JBigDecimal): JBigDecimal = a.abs
    def compare(x: JBigDecimal, y: JBigDecimal): Int = x compareTo y
  }

  /**
   * An implementation of "Quadratic Interval Refinement for Real Roots" by
   * John Abbot for `BigDecimal`.
   */
  def QIR(poly: Polynomial[Rational], lowerBound: Rational, upperBound: Rational, digits: Int): BigDecimalRootRefinement = {

    // TODO: See if Algebraic is faster.
    def evalExact(x: JBigDecimal): JBigDecimal =
      poly(Rational(new BigDecimal(x, MathContext.UNLIMITED)))
        .toBigDecimal(digits, RoundingMode.UP)
        .bigDecimal

    val lb = lowerBound.toBigDecimal(digits, RoundingMode.CEILING).bigDecimal
    val ub = upperBound.toBigDecimal(digits, RoundingMode.FLOOR).bigDecimal

    val qlb = Rational(new BigDecimal(lb, MathContext.UNLIMITED))
    val qub = Rational(new BigDecimal(ub, MathContext.UNLIMITED))

    // Returns true if there is a root in the open sub-interval (l, r).
    def hasRoot(l: Rational, r: Rational): Boolean =
      if (l != r) {
        // Ue Descartes' rule of signs to see if the root in the open interval
        // is actually in the sub interval (l, r).
        val poly0 = poly
          .shift(l)
          .removeZeroRoots
          .reciprocal
          .shift((r - l).reciprocal)
          .removeZeroRoots
        poly0.signVariations % 2 == 1
      } else {
        false
      }

    if (hasRoot(lowerBound, qlb)) {
      BoundedLeft(lowerBound, lb)
    } else if (hasRoot(qub, upperBound)) {
      BoundedRight(ub, upperBound)
    } else {
      QIR(lb, ub, digits)(evalExact)
    }
  }

  /**
   * An implementation of "Quadratic Interval Refinement for Real Roots" by
   * John Abbot for `BigDecimal`.
   */
  def QIR(lowerBound: JBigDecimal, upperBound: JBigDecimal, digits: Int)(evalExact: JBigDecimal => JBigDecimal): BigDecimalRootRefinement = {
    val eps = JBigDecimal.valueOf(1, digits)

    @tailrec
    def loop(
      lx: JBigDecimal,
      ly: JBigDecimal,
      rx: JBigDecimal,
      ry: JBigDecimal,
      n: Int
    ): BigDecimalRootRefinement = {
      val dy = ly.subtract(ry)
      val s = ly.divide(dy, n, RoundingMode.HALF_UP)
      val dx = rx.subtract(lx)
      if (dx.compareTo(eps) <= 0) {
        Bounded(lx, rx)
      } else {
        val delta = dx.multiply(s.ulp)
        val k = s.unscaledValue
        val x1 = lx.add(delta.multiply(new JBigDecimal(k))).setScale(digits, RoundingMode.HALF_UP)
        val y1 = evalExact(x1)
        val s1 = y1.sign
        if (s1 == ly.sign) {
          val x2 = x1.add(delta).setScale(digits, RoundingMode.CEILING)
          val y2 = evalExact(x2)
          val s2 = y2.sign
          if (s2 == s1) loop0(lx, ly, rx, ry)
          else if (s2 == ry.sign) loop(x1, y1, x2, y2, 2 * n)
          else ExactRoot(x2)
        } else if (s1 == ry.sign) {
          val x0 = x1.subtract(delta).setScale(digits, RoundingMode.FLOOR)
          val y0 = evalExact(x0)
          val s0 = y0.sign
          if (s0 == s1) loop0(lx, ly, rx, ry)
          else if (s0 == ly.sign) loop(x0, y0, x1, y1, 2 * n)
          else ExactRoot(x0)
        } else {
          ExactRoot(x1)
        }
      }
    }

    def bisect(
      x0: JBigDecimal,
      y0: JBigDecimal,
      x1: JBigDecimal,
      y1: JBigDecimal,
      x2: JBigDecimal,
      y2: JBigDecimal
    ): BigDecimalRootRefinement = {
      if (y0.signum == 0) ExactRoot(x0)
      else if (y1.signum == 0) ExactRoot(x1)
      else if (y2.signum == 0) ExactRoot(x2)
      else if (y0.sign != y1.sign) loop(x0, y0, x1, y1, 1)
      else loop(x1, y1, x2, y2, 1)
    }

    def loop0(
      x0: JBigDecimal,
      y0: JBigDecimal,
      x5: JBigDecimal,
      y5: JBigDecimal
    ): BigDecimalRootRefinement = {
      val dy = y0.subtract(y5)
      val k = y0
        .divide(dy, 1, RoundingMode.HALF_UP)
        .unscaledValue
        .intValue
      val eps = x5.subtract(x0).divide(new JBigDecimal(5))
      def eval(k: Int): (JBigDecimal, JBigDecimal) = {
        val x = new JBigDecimal(k).multiply(eps).add(x0)
        val y = evalExact(x)
        (x, y)
      }

      if (k < 5) {
        val (x2, y2) = eval(2)
        if (y2.sign != y0.sign) {
          val (x1, y1) = eval(1)
          bisect(x0, y0, x1, y1, x2, y2)
        } else {
          val (x3, y3) = eval(3)
          if (y3.sign == y5.sign) {
            loop(x2, y2, x3, y3, 1)
          } else {
            val (x4, y4) = eval(4)
            bisect(x3, y3, x4, y4, x5, y5)
          }
        }
      } else {
        val (x3, y3) = eval(3)
        if (y3.sign != y5.sign) {
          val (x4, y4) = eval(4)
          bisect(x3, y3, x4, y4, x5, y5)
        } else {
          val (x2, y2) = eval(2)
          if (y2.sign == y0.sign) {
            loop(x2, y2, x3, y3, 1)
          } else {
            val (x1, y1) = eval(1)
            bisect(x0, y0, x1, y1, x2, y2)
          }
        }
      }
    }

    val lx = lowerBound
    val ly = evalExact(lx)
    val rx = upperBound
    val ry = evalExact(rx)
    loop0(lx, ly, rx, ry) match {
      case Bounded(lowerBound, upperBound) =>
        Bounded(
          lowerBound.setScale(digits, RoundingMode.FLOOR),
          upperBound.setScale(digits, RoundingMode.CEILING))
      case root =>
        root
    }
  }
}

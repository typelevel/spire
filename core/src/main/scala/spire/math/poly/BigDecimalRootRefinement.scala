package spire
package math
package poly

import java.math.{BigDecimal => JBigDecimal, RoundingMode, MathContext}

import spire.algebra._
import spire.std.bigDecimal._
import spire.syntax.signed._

case class BigDecimalRootRefinement(
  context: BigDecimalRootRefinement.ApproximationContext,
  approximation: BigDecimalRootRefinement.Approximation
) {
  import BigDecimalRootRefinement._

  def approximateValue: JBigDecimal = {
    import context.{ceil, floor}
    approximation match {
      case ExactRoot(root)         => floor(root)
      case Bounded(lb, _, _, _, n) => ceil(lb)
      case BoundedLeft(_, ub)      => floor(ub)
      case BoundedRight(lb, _)     => ceil(lb)
      case Unbounded(lb, ub)       => ceil(lb)
    }
  }

  def refine(scale: Int): BigDecimalRootRefinement =
    context match {
      case AbsoluteContext(poly, oldScale) if oldScale >= scale =>
        this
      case _ =>
        val newContext = AbsoluteContext(context.poly, scale)
        BigDecimalRootRefinement(newContext, refineApproximation(newContext))
    }

  def refine(mc: MathContext): BigDecimalRootRefinement =
    context match {
      case RelativeContext(poly, oldmc) if oldmc.getPrecision >= mc.getPrecision =>
        this
      case _ =>
        val newContext = RelativeContext(context.poly, mc)
        BigDecimalRootRefinement(newContext, refineApproximation(newContext))
    }

  def refineApproximation(ctx: ApproximationContext): Approximation = approximation match {
    case ExactRoot(_) =>
      approximation
    case Bounded(lb, lby, ub, uby, n) =>
      QIR(ctx, lb, lby, ub, uby, n)
    case BoundedLeft(lb, ubApprox) =>
      val lbApprox = ctx.ceil(lb)
      val ub = Rational(new BigDecimal(ubApprox, MathContext.UNLIMITED))
      QIR(ctx, lb, ub, lbApprox, ubApprox)
    case BoundedRight(lbApprox, ub) =>
      val lb = Rational(new BigDecimal(lbApprox, MathContext.UNLIMITED))
      val ubApprox = ctx.floor(ub)
      QIR(ctx, lb, ub, lbApprox, ubApprox)
    case Unbounded(lb, ub) =>
      val lbApprox = ctx.ceil(lb)
      val ubApprox = ctx.floor(ub)
      QIR(ctx, lb, ub, lbApprox, ubApprox)
  }
}

object BigDecimalRootRefinement {
  def apply(poly: Polynomial[BigDecimal], lowerBound: Rational, upperBound: Rational): BigDecimalRootRefinement = {
    // We expect the polynomial to evaluate with exact arithmetic, so we ditch
    // whatever MathContext the user provided and use an UNLIMITED context. It
    // would probably be best just to use JBigDecimal and avoid the unexpected
    // precision loss inherent with using them.
    val upoly = poly.map { n => new BigDecimal(n.bigDecimal, MathContext.UNLIMITED) }
    BigDecimalRootRefinement(AbsoluteContext(upoly), Unbounded(lowerBound, upperBound))
  }

  def apply(poly: Polynomial[BigDecimal],
            lowerBound: Rational,
            upperBound: Rational,
            scale: Int
  ): BigDecimalRootRefinement =
    apply(poly, lowerBound, upperBound).refine(scale)

  def apply(poly: Polynomial[BigDecimal],
            lowerBound: Rational,
            upperBound: Rational,
            mc: MathContext
  ): BigDecimalRootRefinement =
    apply(poly, lowerBound, upperBound).refine(mc)

  implicit private val JBigDecimalOrder: Signed[JBigDecimal] = new SignedAdditiveAbGroup[JBigDecimal] {
    def negate(x: JBigDecimal) = x.negate
    def zero = JBigDecimal.ZERO
    def plus(x: JBigDecimal, y: JBigDecimal) = x.add(y)
    override def signum(a: JBigDecimal): Int = a.signum
    override def abs(a: JBigDecimal): JBigDecimal = a.abs
    def compare(x: JBigDecimal, y: JBigDecimal): Int = x.compareTo(y)
  }

  private val bits2dec: Double = log(2, 10)

  sealed abstract class Approximation
  case class ExactRoot(root: JBigDecimal) extends Approximation
  case class Bounded(lowerBound: JBigDecimal,
                     lowerBoundValue: JBigDecimal,
                     upperBound: JBigDecimal,
                     upperBoundValue: JBigDecimal,
                     n: Int
  ) extends Approximation
  case class BoundedLeft(lowerBound: Rational, upperBound: JBigDecimal) extends Approximation
  case class BoundedRight(lowerBound: JBigDecimal, upperBound: Rational) extends Approximation
  case class Unbounded(lowerBound: Rational, upperBound: Rational) extends Approximation

  sealed abstract class ApproximationContext {
    def poly: Polynomial[BigDecimal]
    def getEps(x: JBigDecimal): Int
    def evalExact(x: JBigDecimal): JBigDecimal
    def floor(x: Rational): JBigDecimal
    def ceil(x: Rational): JBigDecimal
    def floor(x: JBigDecimal): JBigDecimal
    def ceil(x: JBigDecimal): JBigDecimal
  }

  case class AbsoluteContext private[poly] (poly: Polynomial[BigDecimal], scale: Int = Int.MinValue)
      extends ApproximationContext {
    def getEps(x: JBigDecimal): Int = scale

    def evalExact(x: JBigDecimal): JBigDecimal =
      poly(new BigDecimal(x, MathContext.UNLIMITED)).bigDecimal
        .setScale(scale, RoundingMode.UP)

    def floor(x: Rational): JBigDecimal =
      x.toBigDecimal(scale, RoundingMode.FLOOR).bigDecimal

    def ceil(x: Rational): JBigDecimal =
      x.toBigDecimal(scale, RoundingMode.CEILING).bigDecimal

    def floor(x: JBigDecimal): JBigDecimal =
      x.setScale(scale, RoundingMode.FLOOR)

    def ceil(x: JBigDecimal): JBigDecimal =
      x.setScale(scale, RoundingMode.CEILING)
  }

  case class RelativeContext private[poly] (poly: Polynomial[BigDecimal], mc: MathContext = new MathContext(0))
      extends ApproximationContext {
    def getEps(x: JBigDecimal): Int =
      x.scale - spire.math.ceil(x.unscaledValue.bitLength * bits2dec).toInt + mc.getPrecision + 1

    def evalExact(x: JBigDecimal): JBigDecimal =
      poly(new BigDecimal(x, MathContext.UNLIMITED)).bigDecimal
        .round(mc)

    def floor(x: Rational): JBigDecimal =
      x.toBigDecimal(new MathContext(mc.getPrecision, RoundingMode.FLOOR)).bigDecimal

    def ceil(x: Rational): JBigDecimal =
      x.toBigDecimal(new MathContext(mc.getPrecision, RoundingMode.CEILING)).bigDecimal

    def floor(x: JBigDecimal): JBigDecimal =
      x.round(new MathContext(mc.getPrecision, RoundingMode.FLOOR))

    def ceil(x: JBigDecimal): JBigDecimal =
      x.round(new MathContext(mc.getPrecision, RoundingMode.CEILING))
  }

  /**
   * An implementation of "Quadratic Interval Refinement for Real Roots" by
   * John Abbot for `BigDecimal`.
   */
  private def QIR(
    context: ApproximationContext,
    lowerBound: Rational,
    upperBound: Rational,
    lb: JBigDecimal,
    ub: JBigDecimal
  ): Approximation = {
    import context._

    // We avoid rational arithmetic by using the fact:
    // poly.compose(x + a/b) == poly.compose(x/b).compose(b*x + a)
    // If we first scale the polynomial by b^poly.degree, then the roots
    // remain the same, but the first composition will partially cancel with
    // b^poly.degree, leaving an integer.
    def shift(poly: Polynomial[BigDecimal], h: Rational): Polynomial[BigDecimal] = {
      val n = poly.degree
      poly
        .mapTerms { case Term(coeff, k) =>
          val a = BigDecimal(h.denominator.toBigInteger.pow(n - k), MathContext.UNLIMITED)
          Term(coeff * a, k)
        }
        .compose(
          Polynomial.linear[BigDecimal](BigDecimal(h.denominator.toBigInteger, MathContext.UNLIMITED),
                                        BigDecimal(h.numerator.toBigInteger, MathContext.UNLIMITED)
          )
        )
        .removeZeroRoots
    }

    // Returns a polynomial with the same roots as
    // `poly.compose(Polynomial.linear(s))`, but without using any Rational
    // arithmetic to compute it.
    def mult(poly: Polynomial[BigDecimal], s: Rational): Polynomial[BigDecimal] = {
      // Let s = a/b and n = poly.degree. The idea here is that we can scale
      // poly by b^n, before composing this with the polynomial (1/b)x,
      // followed by composing the result with the polynomial ax. The first 2
      // steps, scaling + composition with (1/b)x, can be simulated by
      // multiplying each term coefficient a_i by b^(n-i) (ie b^n/b^k=b^(n-k)).
      // This rigamarole let's us avoid rational arithmetic. Most importantly,
      // we avoid division, which we cannot do exactly with BigDecimal.
      val n = poly.degree
      poly
        .mapTerms { case Term(coeff, k) =>
          val a = BigDecimal(s.denominator.toBigInteger.pow(n - k), MathContext.UNLIMITED)
          Term(coeff * a, k)
        }
        .compose(Polynomial.linear[BigDecimal](BigDecimal(s.numerator.toBigInteger, MathContext.UNLIMITED)))
        .removeZeroRoots
    }

    // Returns true if there is a root in the open sub-interval (l, r).
    def hasRoot(l: Rational, r: Rational): Boolean =
      if (l != r) {
        // Use Descartes' rule of signs to see if the root in the open interval
        // is actually in the sub interval (l, r). Since Descartes' rule of
        // signs only reports positive roots, we need to first transform the
        // polynomial by mapping the open interval (l, r) to (0, inf). This
        // will ensure that the only positive roots are those in the open
        // interval (l, r).  This is accomplished with following
        // transformations:
        //
        // 1) shifting the polynomial by l by composing it with (x + l)
        // 2) mapping (0, (r - l)) to (0, 1) by composing 1) with (r - l)
        // 3) mapping (0, 1) to (1, inf) by taking the reciprocal of 2)
        // 4) mapping (1, inf) to (0, inf) by composing 3) by x + 1.
        val poly0 = mult(shift(poly, l), r - l).reciprocal.shift(1).removeZeroRoots
        poly0.signVariations % 2 == 1
      } else {
        false
      }

    // QIR expects that (lx,rx) contain exactly 1 root and that they evaluate
    // to different, non-zero signs. However, we may find 1 or both of the
    // bounds to be 0, or to have "overshot" the root, so we have to either
    // find a good (lx, rx) or return an approximation early.
    def adjust(lx: JBigDecimal,
               lyOpt: Option[JBigDecimal],
               rx: JBigDecimal,
               ryOpt: Option[JBigDecimal]
    ): Approximation = {
      def qlx = Rational(new BigDecimal(lx, MathContext.UNLIMITED))
      def qrx = Rational(new BigDecimal(rx, MathContext.UNLIMITED))

      if (lx.compareTo(rx) < 0) {
        val ly = lyOpt.getOrElse(evalExact(lx))
        val ry = ryOpt.getOrElse(evalExact(rx))
        if (ly.signum == 0) {
          if (qlx > lowerBound) {
            // We've "bumped" the lowerbound up to an exact root, coincidentally.
            ExactRoot(lx)
          } else {
            // We try to push lx up a bit to get the sign to change.
            adjust(lx.add(JBigDecimal.valueOf(1, getEps(lx))), None, rx, Some(ry))
          }
        } else if (ry.signum == 0) {
          if (qrx < upperBound) {
            // We've "bumped" the lowerbound up to an exact root, coincidentally.
            ExactRoot(rx)
          } else {
            // We try to push rx down a bit to get the sign to change.
            adjust(lx, Some(ly), rx.subtract(JBigDecimal.valueOf(1, getEps(rx))), None)
          }
        } else if (ry.signum == ly.signum) {
          // We've managed to overshoot the actual root, but since we're still
          // "in-bounds", we know it's in either the left cut off bit or the
          // right.
          if (hasRoot(lowerBound, qlx)) {
            BoundedLeft(lowerBound, lx)
          } else {
            BoundedRight(rx, upperBound)
          }
        } else {
          // Yay! We've successfully approximated the lower/upper bounds with
          // big decimal, while keeping the root within (lx, rx).
          QIR(context, lx, ly, rx, ry)
        }
      } else {
        // We overshot a root.
        Unbounded(lowerBound, upperBound)
      }
    }

    adjust(lb, None, ub, None)
  }

  /**
   * An implementation of "Quadratic Interval Refinement for Real Roots" by
   * John Abbot for `BigDecimal`.
   */
  // scalastyle:off method.length
  private def QIR(
    context: ApproximationContext,
    lowerBound: JBigDecimal,
    lowerBoundValue: JBigDecimal,
    upperBound: JBigDecimal,
    upperBoundValue: JBigDecimal,
    n0: Int = 0
  ): Approximation = {
    import context._

    @tailrec
    def loop(
      lx: JBigDecimal,
      ly: JBigDecimal,
      rx: JBigDecimal,
      ry: JBigDecimal,
      n: Int
    ): Approximation = {
      val dx = rx.subtract(lx)
      val scale = max(getEps(lx), getEps(rx))
      val eps = JBigDecimal.valueOf(1, scale)
      if (dx.compareTo(eps) <= 0) {
        Bounded(lx, ly, rx, ry, n)
      } else {
        val dy = ly.subtract(ry)
        val s = ly.divide(dy, n, RoundingMode.HALF_UP) // BAM!
        val delta = dx.multiply(s.ulp)
        val k = s.unscaledValue
        val x1 = lx.add(delta.multiply(new JBigDecimal(k))) // BAM!
        val y1 = evalExact(x1)
        val s1 = y1.sign
        if (s1 == ly.sign) {
          val x2 = x1.add(delta)
          val y2 = evalExact(x2)
          val s2 = y2.sign
          if (s2 == s1) loop0(lx, ly, rx, ry)
          else if (s2 == ry.sign) loop(x1, y1, x2, y2, 2 * n)
          else ExactRoot(x2)
        } else if (s1 == ry.sign) {
          val x0 = x1.subtract(delta)
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
    ): Approximation = {
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
    ): Approximation = {
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
    val ly = lowerBoundValue
    val rx = upperBound
    val ry = upperBoundValue

    if (n0 <= 0) {
      loop0(lx, ly, rx, ry)
    } else {
      loop(lx, ly, rx, ry, n0)
    }
  }
  // scalastyle:on method.length
}

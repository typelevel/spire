package spire.math
package cf

import spire.math.{SafeLong => Z}
import SafeLong.{zero, one, two, three/*, four*/}

object GCF {

  val four = Z(4)

  type GCF = Stream[(Z, Z)]

  val pi: GCF = {
    def ts(x: Z, y: Z): Stream[(Z, Z)] =
      (x ** 2, y) #:: ts(x + one, y + two)

    (four, one) #:: ts(one, three)
  }

  def toCF(gcf: GCF): CF = {
    def loop(terms: GCF, pold: Z, p: Z, qold: Z, q: Z): CF = {
      val (n, d, tail) = terms match {
        case Stream.Empty => (zero, one, terms)
        case (n, d) #:: t => (n, d, t)
      }
      val pnew = p * d + pold * n
      val qnew = q * d + qold * n
      val g = (pnew gcd qnew) gcd (p gcd q)

      val (pnew1, p1, qnew1, q1) =
        if (g > 1) (pnew / g, p / g, qnew / g, q / g) else (pnew, p, qnew, q)

      val (x0, y0) = p1 /% q1
      val (x1, y1) = pnew1 /% qnew1

      if (x0 == x1) x0 ~: loop(tail, q1, qnew1, y0, y1)
      else loop(tail, p1, pnew1, q1, qnew1)
    }

    loop(gcf, one, zero, zero, one)
  }
}

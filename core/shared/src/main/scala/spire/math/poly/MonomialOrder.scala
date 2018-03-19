package spire
package math
package poly

import scala.annotation.tailrec

import spire.algebra._

/** Total order for monomial satisfying two additional properties:
  * 
  * - ordering respects multiplication: if `u <= v`, then for any monomial `w` we have `u w <= v w`,
  * - if `u` is any monomial then `1 <= u`.
  * 
  * This implies in particular:
  * 
  * - if `u` and `v` are monomials, then `u <= u v`.
  */
trait MonomialOrder extends Order[Monomial]

object MonomialOrder {

  /** Lexicographic order for monomials, e.g. x^2 > xy > xz > x > y^2 > yz > y > z^2 > z > 1. */
  implicit object lex extends MonomialOrder {

    override def eqv(x: Monomial, y: Monomial): Boolean = x == y // simple fallback to the equals method
    override def neqv(x: Monomial, y: Monomial): Boolean = !(x == y)

    def compare(lhs: Monomial, rhs: Monomial): Int = {
      @tailrec def rec(i: Int): Int =
        if (i == lhs.nVariables) { // at the end of lhs
          if (i == rhs.nVariables) // at the end of both
            0
          else
            -1
        } else { // not at the end of lhs
          if (i == rhs.nVariables) // at the end of rhs
            1
          else { // not at the end of both lhs and rhs
            val lv = lhs.variable(i)
            val rv = rhs.variable(i)
            if (lv < rv) 1
            else if (lv > rv) -1
            else { // same variable
              val le = lhs.exponent(i)
              val re = rhs.exponent(i)
              if (le < re) -1
              else if (le > re) 1
              else rec(i + 1) // same variable and same exponent
            }
          }
        }
      rec(0)
    }

  }

  /** Graded lexicographic order for monomials, e.g. x^2 > xy > xz > y^2 > yz > z^2 > x > y > z > 1. */
  implicit object gradLex extends MonomialOrder {

    override def eqv(x: Monomial, y: Monomial): Boolean = x == y // simple fallback to the equals method
    override def neqv(x: Monomial, y: Monomial): Boolean = !(x == y)

    def compare(lhs: Monomial, rhs: Monomial): Int = {
      val ld = lhs.degree
      val rd = rhs.degree
      if (ld > rd) 1
      else if (ld < rd) -1
      else MonomialOrder.lex.compare(lhs, rhs)
    }

  }

  /** Graded reverse lexicographic order for monomials, e.g. x^2 > xy > y^2 > xz > yz > z^2 > x > y > z > 1. */
  implicit object gradRevLex extends MonomialOrder {

    override def eqv(x: Monomial, y: Monomial): Boolean = x == y // simple fallback to the equals method
    override def neqv(x: Monomial, y: Monomial): Boolean = !(x == y)

    def compare(lhs: Monomial, rhs: Monomial): Int = {
      @tailrec def rec(li: Int, ri: Int): Int =
        if (li == -1) { // at the end of lhs
          if (ri == -1) // at the end of rhs
            0
          else
            1
        } else { // not at the end of lhs
          if (ri == -1) // at the end of rhs
            -1
          else { // not at the end of both lhs and rhs
            val lv = lhs.variable(li)
            val rv = rhs.variable(ri)
            if (lv < rv) // rv is the right-most variable
              1 // and lhs.exponent(rv) - rhs.exponent(rv) < 0, so lhs is bigger
            else if (lv > rv)
              -1 //  lv is the right-most variable
            else { // same variable
              val le = lhs.exponent(li)
              val re = rhs.exponent(ri)
              if (le < re) 1
              else if (le > re) -1
              else rec(li - 1, ri - 1) // same variable and same exponent
            }
          }
        }
      val ld = lhs.degree
      val rd = rhs.degree
      if (ld > rd) 1
      else if (ld < rd) -1
      else rec(lhs.nVariables - 1, rhs.nVariables - 1)
    }

  }

}

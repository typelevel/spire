package spire.algebra
package free

import scala.annotation.tailrec

import spire.std.option._
import spire.std.map._
import spire.std.int._
import spire.syntax.rng._

final class FreeAbGroup[A] private (val terms: Map[A, Int]) extends AnyVal { lhs =>

  /**
   * Maps the terms using `f` to type `B` and sums their results using the
   * [[AbGroup]] for `B`.
   */
  def run[B](f: A => B)(implicit B: AbGroup[B]): B =
    terms.foldLeft(B.id) { case (total, (a, n)) =>
      B.op(total, B.combinen(f(a), n))
    }

  /**
   * As long as there are no negative terms, this maps the terms using `f`,
   * then sums the results using the [[CMonoid]] for `B`. If there is a
   * negative term, then `None` is returned.
   */
  def runMonoid[B](f: A => B)(implicit B: CMonoid[B]): Option[B] = {
    val it = terms.iterator

    @tailrec def loop(total: B): Option[B] =
      if (it.hasNext) {
        val (a, n) = it.next()
        if (n < 0) None
        else loop(B.op(total, B.combinen(f(a), n)))
      } else Some(total)

    loop(B.id)
  }

  /**
   * As long as there are no negative terms and at least 1 positive term,
   * this maps the terms using `f`, then sums the results using the
   * [[CSemigroup]] for `B`. If there is a negative term, or if there are
   * no terms at all, then `None` is returned.
   */
  def runSemigroup[B](f: A => B)(implicit B: CSemigroup[B]): Option[B] = {
    val it = terms.iterator

    @tailrec def loop1(total: B): Option[B] =
      if (it.hasNext) {
        val (a, n) = it.next()
        if (n == 0) loop1(total)
        else if (n < 0) None
        else loop1(B.op(total, B.combinen(f(a), n)))
      } else Some(total)

    @tailrec def loop0: Option[B] =
      if (it.hasNext) {
        val (a, n) = it.next()
        if (n == 0) loop0
        else if (n < 0) None
        else loop1(B.combinen(f(a), n))
      } else None

    loop0
  }

  /**
   * Sums up the results of the negative and positive terms separately, using
   * `f` to map the terms to type `B` and using its [[CMonoid]]. This returns
   * a tuple with the sum of the negative terms on the left and the sum of the
   * positive terms on the right.
   */
  def split[B](f: A => B)(implicit B: CMonoid[B]): (B, B) =
    terms.foldLeft((B.id, B.id)) { case ((ltotal, rtotal), (a, n)) =>
      if (n < 0) (B.op(ltotal, B.combinen(f(a), -n)), rtotal)
      else if (n > 0) (ltotal, B.op(rtotal, B.combinen(f(a), n)))
      else (ltotal, rtotal)
    }

  /**
   * Sums up the results of the negative and positive terms separately, using
   * `f` to map the terms to type `B` and using its [[CSemigroup]]. This returns
   * a tuple with the sum of the negative terms on the left and the sum of the
   * positive terms on the right. If either side has no terms at all, then that
   * side is `None`.
   */
  def splitSemigroup[B](f: A => B)(implicit B: CSemigroup[B]): (Option[B], Option[B]) =
    split[Option[B]] { a => Some(f(a)) }

  def |+|(rhs: FreeAbGroup[A]): FreeAbGroup[A] =
    new FreeAbGroup(lhs.terms + rhs.terms)

  def |-|(rhs: FreeAbGroup[A]): FreeAbGroup[A] =
    new FreeAbGroup(lhs.terms - rhs.terms)

  def inverse: FreeAbGroup[A] =
    new FreeAbGroup(-terms)

  override def toString: String =
    if (terms.isEmpty) "e"
    else terms.filter(_._2 != 0).map {
      case (a, n) if n == 1 => a.toString
      case (a, n) if n != 0 => s"($a)^$n"
    }.mkString(" |+| ")
}

object FreeAbGroup { companion =>
  final def id[A]: FreeAbGroup[A] = new FreeAbGroup(Map.empty)

  final def apply[A](a: A): FreeAbGroup[A] = lift(a)
  final def lift[A](a: A): FreeAbGroup[A] = new FreeAbGroup[A](Map((a, 1)))

  implicit def FreeAbGroupGroup[A]: AbGroup[FreeAbGroup[A]] = new AbGroup[FreeAbGroup[A]] {
    def id: FreeAbGroup[A] = companion.id
    def op(a: FreeAbGroup[A], b: FreeAbGroup[A]): FreeAbGroup[A] = a |+| b
    def inverse(a: FreeAbGroup[A]): FreeAbGroup[A] = a.inverse
    override def opInverse(a: FreeAbGroup[A], b: FreeAbGroup[A]): FreeAbGroup[A] = a |-| b
  }
}

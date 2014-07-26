package spire.math

import spire.algebra.Order

class Trilean(val int: Int) extends AnyVal { lhs =>
  def isTrue: Boolean = int == -1
  def isFalse: Boolean = int == 0
  def isUnknown: Boolean = int == 1

  def isKnown: Boolean = int != 1
  def isNotTrue: Boolean = int != -1
  def isNotFalse: Boolean = int != 0

  def fold[A](f: Boolean => A)(g: => A): A =
    if (int == 1) g else f(int == -1)

  def assumeTrue: Boolean =
    int != 0

  def assumeFalse: Boolean =
    int == -1

  def toBoolean(b: => Boolean): Boolean =
    if (int == 1) b else int == -1

  def toOption: Option[Boolean] =
    if (int == 1) None else Some(int == -1)

  override def toString: String =
    if (int == -1) "T" else if (int == 0) "F" else "U"

  def &&(rhs: => Trilean): Trilean =
    if (lhs.int == 0) lhs else lhs & rhs

  def ||(rhs: => Trilean): Trilean =
    if (lhs.int == -1) lhs else lhs | rhs

  def unary_! : Trilean =
    if (int == 1) lhs else new Trilean(~lhs.int)

  def &(rhs: Trilean): Trilean =
    new Trilean(lhs.int & rhs.int)

  def |(rhs: Trilean): Trilean =
    new Trilean(lhs.int | rhs.int)

  def ^(rhs: Trilean): Trilean =
    if (lhs.int == 1) lhs
    else if (rhs.int == 1) rhs
    else new Trilean(lhs.int ^ rhs.int)

  def imp(rhs: Trilean): Trilean =
    (!lhs) | rhs

  def nand(rhs: Trilean): Trilean =
    !(lhs & rhs)

  def nor(rhs: Trilean): Trilean =
    !(lhs | rhs)

  def nxor(rhs: Trilean): Trilean =
    if (lhs.int == 1) lhs
    else if (rhs.int == 1) rhs
    else new Trilean(~(lhs.int ^ rhs.int))
}

object Trilean {
  val True: Trilean = new Trilean(-1)
  val False: Trilean = new Trilean(0)
  val Unknown: Trilean = new Trilean(1)

  def apply(b: Boolean): Trilean = if (b) True else False
}

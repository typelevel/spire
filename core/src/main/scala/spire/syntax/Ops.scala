package spire
package syntax

import spire.algebra._
import spire.algebra.partial._
import spire.math._

final class LiteralIntOrderOps(val lhs: Int) extends AnyVal {
  def <[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lt(c.fromInt(lhs), rhs)
  def <=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lteqv(c.fromInt(lhs), rhs)
  def >[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gt(c.fromInt(lhs), rhs)
  def >=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gteqv(c.fromInt(lhs), rhs)

  def cmp[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Int = ev.compare(c.fromInt(lhs), rhs)
  def min[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.min(c.fromInt(lhs), rhs)
  def max[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.max(c.fromInt(lhs), rhs)
}

final class LiteralLongOrderOps(val lhs: Long) extends AnyVal {
  def <[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lt(c.fromLong(lhs), rhs)
  def <=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lteqv(c.fromLong(lhs), rhs)
  def >[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gt(c.fromLong(lhs), rhs)
  def >=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gteqv(c.fromLong(lhs), rhs)

  def cmp[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Int = ev.compare(c.fromLong(lhs), rhs)
  def min[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.min(c.fromLong(lhs), rhs)
  def max[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.max(c.fromLong(lhs), rhs)
}

final class LiteralDoubleOrderOps(val lhs: Double) extends AnyVal {
  def <[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lt(c.fromDouble(lhs), rhs)
  def <=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.lteqv(c.fromDouble(lhs), rhs)
  def >[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gt(c.fromDouble(lhs), rhs)
  def >=[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Boolean = ev.gteqv(c.fromDouble(lhs), rhs)

  def cmp[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): Int = ev.compare(c.fromDouble(lhs), rhs)
  def min[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.min(c.fromDouble(lhs), rhs)
  def max[A](rhs: A)(implicit ev: Order[A], c: ConvertableTo[A]): A = ev.max(c.fromDouble(lhs), rhs)
}

final class LiteralIntTruncatedDivisionOps(val lhs: Int) extends AnyVal {
  def tquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tquot(c.fromInt(lhs), rhs)
  def tmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tmod(c.fromInt(lhs), rhs)
  def tquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.tquotmod(c.fromInt(lhs), rhs)
  def fquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fquot(c.fromInt(lhs), rhs)
  def fmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fmod(c.fromInt(lhs), rhs)
  def fquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.fquotmod(c.fromInt(lhs), rhs)
}

final class LiteralLongTruncatedDivisionOps(val lhs: Long) extends AnyVal {
  def tquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tquot(c.fromLong(lhs), rhs)
  def tmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tmod(c.fromLong(lhs), rhs)
  def tquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.tquotmod(c.fromLong(lhs), rhs)
  def fquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fquot(c.fromLong(lhs), rhs)
  def fmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fmod(c.fromLong(lhs), rhs)
  def fquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.fquotmod(c.fromLong(lhs), rhs)
}

final class LiteralDoubleTruncatedDivisionOps(val lhs: Double) extends AnyVal {
  def tquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tquot(c.fromDouble(lhs), rhs)
  def tmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.tmod(c.fromDouble(lhs), rhs)
  def tquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.tquotmod(c.fromDouble(lhs), rhs)
  def fquot[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fquot(c.fromDouble(lhs), rhs)
  def fmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): A = ev.fmod(c.fromDouble(lhs), rhs)
  def fquotmod[A](rhs: A)(implicit ev: TruncatedDivision[A], c: ConvertableTo[A]): (A, A) =
    ev.fquotmod(c.fromDouble(lhs), rhs)
}

final class GroupoidCommonOps[A](lhs: A)(implicit ev: Groupoid[A]) {
  def inverse: A = ev.inverse(lhs)
  def isId(implicit ev1: Eq[A]): Boolean = ev.isId(lhs)(ev1)
}


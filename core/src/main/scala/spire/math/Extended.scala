package spire.math

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

sealed trait Extended[A] { lhs =>

  import Extended.{PosInf, Fin, NegInf}

  def getOrError(): A = this match {
    case Fin(a) => a
    case _ => throw new ArithmeticException(toString)
  }

  def <(rhs: Extended[A])(implicit ev: Order[A]): Boolean =
    (lhs compare rhs) < 0
  def <=(rhs: Extended[A])(implicit ev: Order[A]): Boolean =
    (lhs compare rhs) <= 0
  def >(rhs: Extended[A])(implicit ev: Order[A]): Boolean =
    (lhs compare rhs) > 0
  def >=(rhs: Extended[A])(implicit ev: Order[A]): Boolean =
    (lhs compare rhs) >= 0

  def compare(rhs: Extended[A])(implicit ev: Order[A]): Int =
    lhs match {
      case PosInf() => rhs match {
        case PosInf() => 0
        case _ => 1
      }
      case Fin(n1) => rhs match {
        case PosInf() => -1
        case Fin(n2) => n1 compare n2
        case NegInf() => 1
      }
      case NegInf() => rhs match {
        case NegInf() => 0
        case _ => -1
      }
    }

  def abs(implicit ev: AdditiveGroup[A], s: Signed[A]): Extended[A] =
    lhs match {
      case NegInf() => PosInf()
      case Fin(n) => if (n.signum < 0) Fin(-n) else this
      case _ => this
    }

  def max(rhs: Extended[A])(implicit ev: Order[A]): Extended[A] =
    if (lhs > rhs) lhs else rhs

  def min(rhs: Extended[A])(implicit ev: Order[A]): Extended[A] =
    if (lhs < rhs) lhs else rhs

  def unary_-(implicit ev: AdditiveGroup[A]): Extended[A] =
    lhs match {
      case PosInf() => NegInf()
      case Fin(n) => Fin(-n)
      case NegInf() => PosInf()
    }

  def +(rhs: Extended[A])(implicit ev: AdditiveMonoid[A]): Extended[A] =
    lhs match {
      case PosInf() => rhs match {
        case NegInf() => Fin(ev.zero)
        case _ => PosInf()
      }
      case Fin(n1) => rhs match {
        case Fin(n2) => Fin(n1 + n2)
        case x => x
      }
      case NegInf() => rhs match {
        case PosInf() => Fin(ev.zero)
        case _ => NegInf()
      }
    }

  def -(rhs: Extended[A])(implicit ev: AdditiveGroup[A]): Extended[A] =
    lhs + (-rhs)

  def *(rhs: Extended[A])(implicit ev: MultiplicativeSemigroup[A], s: Signed[A]): Extended[A] =
    lhs match {
      case PosInf() => rhs match {
        case NegInf() => rhs
        case Fin(n2) => if (n2.signum < 0) NegInf() else lhs
        case _ => lhs
      }
      case Fin(n1) => rhs match {
        case PosInf() => if (n1.signum < 0) NegInf() else rhs
        case Fin(n2) => Fin(n1 * n2)
        case NegInf() =>if (n1.signum < 0) PosInf() else rhs
      }
      case NegInf() => rhs match {
        case PosInf() => lhs
        case Fin(n2) => if (n2.signum < 0) PosInf() else lhs
        case NegInf() => PosInf()
      }
    }

  def /~(rhs: Extended[A])(implicit ev: EuclideanRing[A], s: Signed[A]): Extended[A] =
    lhs match {
      case PosInf() => rhs match {
        case PosInf() => Fin(ev.one)
        case Fin(n2) => if (n2.signum < 0) NegInf() else PosInf()
        case NegInf() => Fin(-ev.one)
      }
      case Fin(n1) => rhs match {
        case PosInf() => Fin(ev.zero)
        case Fin(n2) => if (n2.signum != 0) Fin(n1 /~ n2) else if (n1.signum < 0) NegInf() else PosInf()
        case NegInf() => Fin(ev.zero)
      }
      case NegInf() => rhs match {
        case PosInf() => Fin(-ev.one)
        case Fin(n2) => if (n2.signum < 0) PosInf() else NegInf()
        case NegInf() => Fin(ev.one)
      }
    }

  def reciprocal(implicit ev: Field[A], s: Signed[A]): Extended[A] =
    lhs match {
      case PosInf() => Fin(ev.zero)
      case Fin(n) => if (n.signum == 0) PosInf() else Fin(n.reciprocal)
      case NegInf() => Fin(ev.zero)
    }

  def /(rhs: Extended[A])(implicit ev: Field[A], s: Signed[A]): Extended[A] =
    lhs match {
      case PosInf() => rhs match {
        case PosInf() => Fin(ev.one)
        case Fin(n2) => if (n2.signum < 0) NegInf() else PosInf()
        case NegInf() => Fin(-ev.one)
      }
      case Fin(n1) => rhs match {
        case PosInf() => Fin(ev.zero)
        case Fin(n2) => if (n2.signum != 0) Fin(n1 / n2) else if (n1.signum < 0) NegInf() else PosInf()
        case NegInf() => Fin(ev.zero)
      }
      case NegInf() => rhs match {
        case PosInf() => Fin(-ev.one)
        case Fin(n2) => if (n2.signum < 0) PosInf() else NegInf()
        case NegInf() => Fin(ev.one)
      }
    }

  def pow(rhs: Int)(implicit ev: NRoot[A], r: Ring[A]): Extended[A] =
    if (rhs < 0) throw new IllegalArgumentException(rhs.toString)
    else if (rhs == 0) Fin(r.one)
    else if (rhs == 1) lhs
    else lhs match {
      case NegInf() => if ((rhs & 1) == 0) PosInf() else lhs
      case Fin(n) => Fin(n.pow(rhs))
      case PosInf() => lhs
    }
}

object Extended {
  def zero[A](implicit ev: AdditiveMonoid[A]): Extended[A] = Fin(ev.zero)
  def one[A](implicit ev: MultiplicativeMonoid[A]): Extended[A] = Fin(ev.one)

  def positiveInfinity[A]: Extended[A] = PosInf()
  def apply[A](a: A): Extended[A] = Fin(a)
  def negativeInfinity[A]: Extended[A] = PosInf()

  case class PosInf[A]() extends Extended[A]
  case class Fin[A](a: A) extends Extended[A]
  case class NegInf[A]() extends Extended[A]
}

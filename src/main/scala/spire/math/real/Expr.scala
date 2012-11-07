package spire.math.real

import spire.math.fpf.MaybeDouble

import spire.math._

/**
 * An `Expr` describes a simple structure for algebraic expressions. Generally,
 * a type `Expr[A]` indicates that `A` has some structure that mirrors `Expr`.
 * To get at this symmetry, you must use the type class `Coexpr[A]`. This let's
 * us switch between types `A` and `Expr[A]`, giving us the ability to extract
 * structure from a type `A`, traverse, and construct the expression tree of
 * `A`, without having to deal with `A` as a concrete type.
 *
 * Using `Coexpr` let's us provide further general constructors and pattern
 * matchers. These are defined as the same name as the case class, but without
 * the `Expr` appended. So, we can, for example, pattern match on an instance
 * `a` of the generic type `A`. Suppose we wanted to map patterns like ab + ac
 * to a(b + c), then we could do the following:
 *
 *   a match {
 *     case Add(Mul(a, b), Mul(c, d)) if a == c => Mul(a, Add(b, d))
 *     case _ => a
 *   }
 *
 */
sealed trait Expr[A]
case class IntExpr[A](n: Int) extends Expr[A]
case class BigIntExpr[A](n: BigInt) extends Expr[A]
case class NegExpr[A](sub: A) extends Expr[A]
case class KRootExpr[A](sub: A, k: Int) extends Expr[A]
case class AddExpr[A](lhs: A, rhs: A) extends Expr[A]
case class SubExpr[A](lhs: A, rhs: A) extends Expr[A]
case class MulExpr[A](lhs: A, rhs: A) extends Expr[A]
case class DivExpr[A](lhs: A, rhs: A) extends Expr[A]

object Expr {
  def apply[A: Coexpr](n: Int): A = IntLit(n)
  def apply[A: Coexpr](n: Long): A = apply(BigInt(n))
  def apply[A: Coexpr](n: BigInt): A = if (n.isValidInt) {
    IntLit(n.toInt)
  } else {
    BigIntLit(n)
  }
  def apply[A: Coexpr](n: Rational): A = Div(apply[A](n.numerator), apply[A](n.denominator))
  def apply[A: Coexpr](n: Double): A = apply[A](Rational(n.toString))
  def apply[A: Coexpr](n: BigDecimal): A = apply[A](Rational(n))

  def toExprString[A: Coexpr](a: A): String = a match {
    case IntLit(n) => n.toString
    case BigIntLit(n) => n.toString
    case Add(a, b) => "%s + %s" format (toExprString(a), toExprString(b))
    case Sub(a, b) => "%s - %s" format (toExprString(a), toExprString(b))
    case Mul(a, b) => "(%s) * (%s)" format (toExprString(a), toExprString(b))
    case Div(a, b) => "(%s) / (%s)" format (toExprString(a), toExprString(b))
    case Neg(a) => "-%s" format toExprString(a)
    case KRoot(a, k) =>
      if (k == 2) {
        "sqrt(%s)" format (toExprString(a))
      } else {
        "%d-root(%s)" format (k, toExprString(a))
      }
  }
}


/**
 * A type class that indicates that the type `A` has a structure that can be
 * modelled by an `Expr[A]`. The type class let's us switch between the 2 types,
 * so that we can both traverse the type `A` as an `Expr` and also construct an
 * `A` from an `Expr[A]`.
 *
 * If `ce` is an instance of `Coexpr[A]`, then ce.coexpr(ce.expr(a)) == a.
 */
trait Coexpr[A] {
  def expr(a: A): Expr[A]
  def coexpr(e: Expr[A]): A
}

object Coexpr {
  def apply[A](implicit ev: Coexpr[A]): Coexpr[A] = ev

  implicit def CoexprOps[A: Coexpr](a: A) = new {
    def expr: Expr[A] = Coexpr[A].expr(a)
  }

  implicit def ExprOps[A: Coexpr](e: Expr[A]) = new {
    def coexpr: A = Coexpr[A].coexpr(e)
  }

  def mirror[A: Coexpr, B: Coexpr](a: A): B = a match {
    case IntLit(n) => IntLit[B](n)
    case BigIntLit(n) => BigIntLit[B](n)
    case Neg(a) => Neg[B](mirror[A,B](a))
    case KRoot(a, k) => KRoot[B](mirror[A,B](a), k)
    case Add(a, b) => Add[B](mirror[A,B](a), mirror[A,B](b))
    case Sub(a, b) => Sub[B](mirror[A,B](a), mirror[A,B](b))
    case Mul(a, b) => Mul[B](mirror[A,B](a), mirror[A,B](b))
    case Div(a, b) => Div[B](mirror[A,B](a), mirror[A,B](b))
  }
}


object IntLit {
  def apply[A: Coexpr](n: Int): A = Coexpr[A].coexpr(IntExpr[A](n))
  def unapply[A: Coexpr](e: A): Option[Int] = Coexpr[A].expr(e) match {
    case IntExpr(n) => Some(n)
    case _ => None
  }
}

object BigIntLit {
  def apply[A: Coexpr](n: BigInt): A = Coexpr[A].coexpr(BigIntExpr[A](n))
  def unapply[A: Coexpr](e: A): Option[BigInt] = Coexpr[A].expr(e) match {
    case BigIntExpr(n) => Some(n)
    case _ => None
  }
}

object Neg {
  def apply[A: Coexpr](a: A): A = Coexpr[A].coexpr(NegExpr[A](a))
  def unapply[A: Coexpr](e: A): Option[A] = Coexpr[A].expr(e) match {
    case NegExpr(s) => Some(s)
    case _ => None
  }
}

object KRoot {
  def apply[A: Coexpr](a: A, k: Int): A = Coexpr[A].coexpr(KRootExpr[A](a, k))
  def unapply[A: Coexpr](e: A): Option[(A,Int)] = Coexpr[A].expr(e) match {
    case KRootExpr(a, k) => Some((a, k))
    case _ => None
  }
}

object Add {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(AddExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case AddExpr(a, b) => Some((a, b))
    case _ => None
  }
}

object Sub {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(SubExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case SubExpr(a, b) => Some((a, b))
    case _ => None
  }
}

object Mul {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(MulExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case MulExpr(a, b) => Some((a, b))
    case _ => None
  }
}

object Div {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(DivExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case DivExpr(a, b) => Some((a, b))
    case _ => None
  }
}



package numerics.math.real

import numerics.math.fpf.MaybeDouble


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
}


object IntLit2 {
  def apply[A: Coexpr](n: Int): A = Coexpr[A].coexpr(IntExpr[A](n))
  def unapply[A: Coexpr](e: A): Option[Int] = Coexpr[A].expr(e) match {
    case IntExpr(n) => Some(n)
    case _ => None
  }
}

object BigIntLit2 {
  def apply[A: Coexpr](n: BigInt): A = Coexpr[A].coexpr(BigIntExpr[A](n))
  def unapply[A: Coexpr](e: A): Option[BigInt] = Coexpr[A].expr(e) match {
    case BigIntExpr(n) => Some(n)
    case _ => None
  }
}

object Neg2 {
  def apply[A: Coexpr](a: A): A = Coexpr[A].coexpr(NegExpr[A](a))
  def unapply[A: Coexpr](e: A): Option[A] = Coexpr[A].expr(e) match {
    case NegExpr(s) => Some(s)
    case _ => None
  }
}

object KRoot2 {
  def apply[A: Coexpr](a: A, k: Int): A = Coexpr[A].coexpr(KRootExpr[A](a, k))
  def unapply[A: Coexpr](e: A): Option[(A,Int)] = Coexpr[A].expr(e) match {
    case KRootExpr(a, k) => Some((a, k))
    case _ => None
  }
}

object Add2 {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(AddExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case AddExpr(a, b) => Some((a, b))
    case _ => None
  }
}

object Sub2 {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(SubExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case SubExpr(a, b) => Some((a, b))
    case _ => None
  }
}

object Mul2 {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(MulExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case MulExpr(a, b) => Some((a, b))
    case _ => None
  }
}

object Div2 {
  def apply[A: Coexpr](a: A, b: A): A = Coexpr[A].coexpr(DivExpr(a, b))
  def unapply[A: Coexpr](e: A): Option[(A,A)] = Coexpr[A].expr(e) match {
    case DivExpr(a, b) => Some((a, b))
    case _ => None
  }
}


trait RealLike2[A] { self: A =>
  implicit def coexpr: Coexpr[A]

  def +(that: A): A = Add2(this, that)
  def -(that: A): A = Sub2(this, that)
  def *(that: A): A = Mul2(this, that)
  def /(that: A): A = Div2(this, that)
  def nroot(k: Int): A = KRoot2[A](this, k)
  def unary_-(): A = Neg2[A](this)
}

trait Real2Transform[A] extends RealLike2[A] { self: A =>
  def transform(a: A): A = a

  override def +(that: A): A = transform(super.+(that))
  override def -(that: A): A = transform(super.-(that))
  override def *(that: A): A = transform(super.*(that))
  override def /(that: A): A = transform(super./(that))
  override def nroot(k: Int): A = transform(super.nroot(k)) 
  override def unary_-(): A = transform(super.unary_-())
}

trait Real2ConstantFolder[A] extends Real2Transform[A] { self: A =>
  override def transform(a: A): A = super.transform(a) match {
    case Add2(IntLit2(x), IntLit2(y)) => IntLit2(x + y)
    case a => a
  }
}

trait Real2FPFilter[A <: Real2FPFilter[A]] extends RealLike2[A] { self: A =>
  lazy val fpf: MaybeDouble = this match {
    case Add2(a, b) => a.fpf + b.fpf
    case Sub2(a, b) => a.fpf - b.fpf
    case Mul2(a, b) => a.fpf * b.fpf
    case Div2(a, b) => a.fpf / b.fpf
    case KRoot2(a, k) => a.fpf nroot k
    case Neg2(a) => -(a.fpf)
    case IntLit2(n) => MaybeDouble(n)
    case BigIntLit2(n) => MaybeDouble(n)
  }
}

final class Real2 private (val expr: Expr[Real2]) extends RealLike2[Real2]
                                                     with Real2FPFilter[Real2]
                                                     with Real2ConstantFolder[Real2] {
  val coexpr = Real2.Real2Coexpr
}

object Real2 {

  def apply(n: Int): Real2 = new Real2(IntExpr[Real2](n))
  def apply(n: BigInt): Real2 = new Real2(BigIntExpr[Real2](n))

  implicit object Real2Coexpr extends Coexpr[Real2] {
    def expr(r: Real2): Expr[Real2] = r.expr
    def coexpr(e: Expr[Real2]): Real2 = new Real2(e)
  }
}



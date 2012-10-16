package spire.example

import shapeless.{ Field => _, _ }
import spire.algebra._
import spire.math._


trait Div[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait DivAux[A <: Nat, B <: Nat, C <: Nat]

object Div {
  implicit def div[A <: Nat, B <: Nat, C <: Nat]
    (implicit div: DivAux[A, B, C]) = new Div[A, B] {
      type Out = C
    }
}

object DivAux {
  import Nat._
  import LT._

  // (a + 1) / b => (a + 1 - b + b) / b => (a + 1 -b ) / b + 1

  implicit def div1[A <: Nat] = new DivAux[_0, A, _0] {}

  implicit def div2[A <: Nat, B <: Nat](implicit e: A < B) =
    new DivAux[A, B, _0] {}

  implicit def div3[A <: Nat, B <: Nat, C <: Nat, D <: Nat]
    (implicit diff: DiffAux[Succ[A], B, C], div: DivAux[C, B, D]) =
      new DivAux[Succ[A], B, Succ[D]] {}
}


/** Typeclass witnessing that `B` divides `A`. */
trait Divisible[A <: Nat, B <: Nat]

object Divisible {
  implicit def divisible[A <: Nat, B <: Nat, C <: Nat]
    (implicit div: DivAux[A, B, C], prod: ProdAux[C, B, A]) =
      new Divisible[A, B] {}
}


/** Does something like this already exist somewhere? */
trait Not[A]

object Not {
  implicit def notAmbig[A](implicit a: A) = new Not[A] {}
  implicit def not[A] = new Not[A] {}
}


/**
 * Typeclass witnessing that `N` is prime.
 */
trait IsPrime[N <: Nat]

/**
 * Typeclass witnessing that `A` has no divisors between _2
 * and `B` inclusive. A number `A` is prime if an
 * `IsPrimeAux[A, A / _2]` exists.
 */
trait IsPrimeAux[A <: Nat, B <: Nat]

object IsPrime {
  import Nat._

  implicit def prime[A <: Nat, B <: Nat]
    (implicit div: DivAux[A, _2, B], p: IsPrimeAux[A, B]) = new IsPrime[A] {}

  implicit def apply[A <: Nat: ToInt: IsPrime] = Nat.toInt[A]
}

object IsPrimeAux {
  import Nat._
  import LT._

  implicit def prime1[A <: Nat](implicit lt: _1 < A) = new IsPrimeAux[A, _1] {}

  implicit def primeN[A <: Nat, B <: Nat]
    (implicit p: IsPrimeAux[A, B], e: Not[Divisible[A, Succ[B]]]) =
      new IsPrimeAux[A, Succ[B]] {}
}


/**
 * A simple class for representing integers mod `N`.
 */
final class Z[N <: Nat] private (val n: Int) /*extends AnyVal*/ {

  // Remove if extending AnyVal.
  override def equals(obj: Any) = obj match {
    case that: Z[_] => that.n == n
    case _ => false
  }

  override def toString = "Z(%d)" format n
}


object Z {

  def unapply(n: Z[_]) = Some(n.n)

  def apply[A <: Nat](n: Int)(implicit toInt: ToInt[A]): Z[A] = {
    val k = (if (n < 0) -n else n) % toInt()
    new Z[A](k)
  }

  implicit def eq[N <: Nat] = new Eq[Z[N]] {
    def eqv(a: Z[N], b: Z[N]) = a.n == b.n
  }

  // TODO: Move field to lower priority for Rings.

   implicit def ring[N <: Nat](implicit ord: ToInt[N]): Ring[Z[N]] = new FiniteRing[N] {
    val toInt: ToInt[N] = ord
  }

  implicit def field[N <: Nat]
    (implicit ord: ToInt[N], p: IsPrime[N]): Field[Z[N]] = new FiniteField[N] {
      val toInt: ToInt[N] = implicitly
    }
}

trait FiniteRing[N <: Nat] extends Ring[Z[N]] {
  implicit def toInt: ToInt[N]

  def order: Int = toInt()

  lazy val zero = Z[N](0)
  lazy val one = Z[N](1)
  def negate(a: Z[N]) = Z[N](order - a.n)
  def plus(a: Z[N], b: Z[N]) = Z[N](a.n + b.n)
  def times(a: Z[N], b: Z[N]) = Z[N](a.n * b.n)
}

trait FiniteField[N <: Nat] extends FiniteRing[N] with Field[Z[N]] {
  import spire.implicits._

  def quot(a: Z[N], b: Z[N]) = Z[N](a.n / b.n)
  def mod(a: Z[N], b: Z[N]) = Z[N](a.n % b.n)

  override def reciprocal(a: Z[N]): Z[N] = if (a === zero) {
    throw new ArithmeticException("Divide by zero.")
  } else (1 until order) map (Z[N](_)) find { b =>
    times(a, b) === one
  } getOrElse {
    // If this is had through Z.field, then this won't happen.
    throw new ArithmeticException("Order isn't prime.")
  }

  def div(a: Z[N], b: Z[N]) = times(a, reciprocal(b))
  def isWhole(a: Z[N]) = true
}


/**
 * An example of type safe finite fields.
 */
object FiniteFieldExample extends App {
  import spire.implicits._
  import Nat._

  // Some examples of primes. Primality testing is currently rather slow.

  implicitly[IsPrime[_2]]
  implicitly[IsPrime[_3]]
  implicitly[IsPrime[_5]]
  implicitly[IsPrime[_13]]
  implicitly[Not[IsPrime[_9]]]
  implicitly[Not[IsPrime[_1]]]


  // If the order is not prime, then we only have a Ring available.

  implicitly[Ring[Z[_9]]]
  implicitly[Not[Field[Z[_9]]]]

  val a = Z[_9](3)
  val b = Z[_9](5)

  assert((a + b) === Z[_9](8))
  assert((a * b) === Z[_9](6))
  // assert((z / b) == Z[_9](6))
  // ^ Compile time error, because _9 is not prime.


  // If the order is prime, then we have a Field and can use division.

  implicitly[Field[Z[_11]]]
  implicitly[Field[Z[_3]]]

  val c = Z[_11](8)
  val d = Z[_11](9)
  
  assert((c + d) === Z[_11](6))
  assert((c * d) === Z[_11](6))
  assert((c / d) === Z[_11](7))

  val e = Z[_3](1)
  val f = Z[_3](2)
  
  assert((e + f) === Z[_3](0))
  assert((e * f) === Z[_3](2))
  assert((e / f) === Z[_3](2))
}


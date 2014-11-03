package spire.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec


object Additive {
  def apply[A](s: Semigroup[A]): AdditiveSemigroup[A] = new AdditiveSemigroup[A] {
    def plus(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](s: CSemigroup[A]): AdditiveCSemigroup[A] = new AdditiveCSemigroup[A] {
    def plus(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](m: Monoid[A]): AdditiveMonoid[A] = new AdditiveMonoid[A] {
    def plus(x: A, y: A): A = m.op(x, y)
    def zero = m.id
  }

  def apply[A](m: CMonoid[A]): AdditiveCMonoid[A] = new AdditiveCMonoid[A] {
    def plus(x: A, y: A): A = m.op(x, y)
    def zero = m.id
  }

  def apply[A](g: Group[A]): AdditiveGroup[A] = new AdditiveGroup[A] {
    def plus(x: A, y: A): A = g.op(x, y)
    override def minus(x: A, y: A): A = g.op(x, g.inverse(y))
    def zero: A = g.id
    def negate(x: A): A = g.inverse(x)
  }

  def apply[A](g: AbGroup[A]): AdditiveAbGroup[A] = new AdditiveAbGroup[A] {
    def plus(x: A, y: A): A = g.op(x, y)
    override def minus(x: A, y: A): A = g.op(x, g.inverse(y))
    def zero: A = g.id
    def negate(x: A): A = g.inverse(x)
  }
}

trait AdditiveSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] {
  def additive: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = plus(x, y)
  }

  def plus(x: A, y: A): A

  /**
   * Return `a` added with itself `n` times.
   */
  def sumn(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated summation for semigroups must have repetitions > 0")
    else if (n == 1) a
    else sumnAboveOne(a, n)

  protected def sumnAboveOne(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) {
        plus(b, extra)
      } else {
        val x = if ((k & 1) == 1) plus(b, extra) else extra
        loop(plus(b, b), k >>> 1, x)
      }
    loop(a, n - 1, a)
  }

  /**
   *  Given a sequence of `as`, sum them using the semigroup and return the total.
   * 
   *  If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  def sumOption(as: TraversableOnce[A]): Option[A] = as.reduceOption(plus)
}

trait AdditiveCSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveSemigroup[A] {
  override def additive: CSemigroup[A] = new CSemigroup[A] {
    def op(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveSemigroup[A] {
  override def additive: Monoid[A] = new Monoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }

  def zero: A

  /**
    * Tests if `a` is zero.
    */
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)

  /**
   * Return `a` added with itself `n` times.
   */
  override def sumn(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated summation for monoids must have repetitions >= 0")
    else if (n == 0) zero
    else if (n == 1) a
    else sumnAboveOne(a, n)

  /**
   *  Given a sequence of `as`, sum them using the monoid and return the total.
   */
  def sum(as: TraversableOnce[A]): A = as.aggregate(zero)(plus, plus)
}

trait AdditiveCMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveMonoid[A] with AdditiveCSemigroup[A] {
  override def additive: CMonoid[A] = new CMonoid[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveMonoid[A] {
  override def additive: Group[A] = new Group[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))

  /**
   * Return `a` added with itself `n` times.
   */
  override def sumn(a: A, n: Int): A =
    if (n == Int.MinValue) plus(sumn(negate(a), Int.MaxValue), negate(a))
    else if (n < 0) sumn(negate(a), -n)
    else if (n == 0) zero
    else if (n == 1) a
    else sumnAboveOne(a, n)
}

trait AdditiveAbGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends AdditiveGroup[A] with AdditiveCMonoid[A] {
  override def additive: AbGroup[A] = new AbGroup[A] {
    def id = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }
}

package spire.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec

object Multiplicative {
  def apply[A](s: Semigroup[A]): MultiplicativeSemigroup[A] = new MultiplicativeSemigroup[A] {
    def times(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](s: CSemigroup[A]): MultiplicativeCSemigroup[A] = new MultiplicativeCSemigroup[A] {
    def times(x: A, y: A): A = s.op(x, y)
  }

  def apply[A](m: Monoid[A]): MultiplicativeMonoid[A] = new MultiplicativeMonoid[A] {
    def times(x: A, y: A): A = m.op(x, y)
    def one = m.id
  }

  def apply[A](m: CMonoid[A]): MultiplicativeCMonoid[A] = new MultiplicativeCMonoid[A] {
    def times(x: A, y: A): A = m.op(x, y)
    def one = m.id
  }

  def apply[A](g: Group[A]): MultiplicativeGroup[A] = new MultiplicativeGroup[A] {
    def times(x: A, y: A): A = g.op(x, y)
    def div(x: A, y: A): A = g.op(x, g.inverse(y))
    def one: A = g.id
    override def reciprocal(x: A): A = g.inverse(x)
  }

  def apply[A](g: AbGroup[A]): MultiplicativeAbGroup[A] = new MultiplicativeAbGroup[A] {
    def times(x: A, y: A): A = g.op(x, y)
    def div(x: A, y: A): A = g.op(x, g.inverse(y))
    def one: A = g.id
    override def reciprocal(x: A): A = g.inverse(x)
  }
}

trait MultiplicativeSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def multiplicative: Semigroup[A] = new Semigroup[A] {
    def op(x: A, y: A): A = times(x, y)
  }

  def times(x: A, y: A): A

  /**
   * Return `a` multiplied with itself `n` times.
   */
  def prodn(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated multiplication for semigroups must have reptitions > 0")
    else if (n == 1) a
    else prodnAboveOne(a, n)

  protected def prodnAboveOne(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) {
        times(b, extra)
      } else {
        val x = if ((k & 1) == 1) times(b, extra) else extra
        loop(times(b, b), k >>> 1, x)
      }
    loop(a, n - 1, a)
  }

  /**
   *  Given a sequence of `as`, sum them using the semigroup and return the total.
   *
   *  If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  def prodOption(as: TraversableOnce[A]): Option[A] = as.reduceOption(times)
}

trait MultiplicativeCSemigroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeSemigroup[A] {
  override def multiplicative: CSemigroup[A] = new CSemigroup[A] {
    def op(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeSemigroup[A] {
  override def multiplicative: Monoid[A] = new Monoid[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
  }

  def one: A

  def isOne(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, one)

  /**
   * Return `a` multiplied with itself `n` times.
   */
  override def prodn(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated multiplication for monoids must have reptitions >= 0")
    else if (n == 0) one
    else if (n == 1) a
    else prodnAboveOne(a, n)

  /**
   *  Given a sequence of `as`, sum them using the monoid and return the total.
   */
  def prod(as: TraversableOnce[A]): A = as.aggregate(one)(times, times)
}

trait MultiplicativeCMonoid[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeMonoid[A] with MultiplicativeCSemigroup[A] {
  override def multiplicative: CMonoid[A] = new CMonoid[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeMonoid[A] {
  override def multiplicative: Group[A] = new Group[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }

  def reciprocal(x: A): A = div(one, x)
  def div(x: A, y: A): A

    /**
   * Return `a` multiplicated with itself `n` times.
   */
  override def prodn(a: A, n: Int): A =
    if (n == Int.MinValue) times(prodn(reciprocal(a), Int.MaxValue), reciprocal(a))
    else if (n < 0) prodn(reciprocal(a), -n)
    else if (n == 0) one
    else if (n == 1) a
    else prodnAboveOne(a, n)
}

trait MultiplicativeAbGroup[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeGroup[A] with MultiplicativeCMonoid[A] {
  override def multiplicative: AbGroup[A] = new AbGroup[A] {
    def id = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }
}

package spire
package algebra


object Multiplicative {
  def apply[A](s: Semigroup[A]): MultiplicativeSemigroup[A] = new MultiplicativeSemigroup[A] {
    def times(x: A, y: A): A = s.combine(x, y)
  }

  def apply[A](s: CSemigroup[A]): MultiplicativeCSemigroup[A] = new MultiplicativeCSemigroup[A] {
    def times(x: A, y: A): A = s.combine(x, y)
  }

  def apply[A](m: Monoid[A]): MultiplicativeMonoid[A] = new MultiplicativeMonoid[A] {
    def times(x: A, y: A): A = m.combine(x, y)
    def one: A = m.empty
  }

  def apply[A](m: CMonoid[A]): MultiplicativeCMonoid[A] = new MultiplicativeCMonoid[A] {
    def times(x: A, y: A): A = m.combine(x, y)
    def one: A = m.empty
  }

  def apply[A](g: Group[A]): MultiplicativeGroup[A] = new MultiplicativeGroup[A] {
    def times(x: A, y: A): A = g.combine(x, y)
    def div(x: A, y: A): A = g.combine(x, y)
    def one: A = g.empty
    override def reciprocal(x: A): A = g.inverse(x)
  }

  def apply[A](g: AbGroup[A]): MultiplicativeAbGroup[A] = new MultiplicativeAbGroup[A] {
    def times(x: A, y: A): A = g.combine(x, y)
    def div(x: A, y: A): A = g.remove(x, y)
    def one: A = g.empty
    override def reciprocal(x: A): A = g.inverse(x)
  }
}

trait MultiplicativeSemigroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def multiplicative: Semigroup[A] = new Semigroup[A] {
    def combine(x: A, y: A): A = times(x, y)
  }

  def times(x: A, y: A): A

  /**
   * Return `a` multiplied with itself `n` times.
   */
  def pow(a: A, n: Int): A =
    if (n > 0) positivePow(a, n)
    else throw new IllegalArgumentException("Illegal non-positive exponent to pow: %s" format n)

  protected[this] def positivePow(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) times(b, extra) else {
        val x = if ((k & 1) == 1) times(b, extra) else extra
        loop(times(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }
  /**
   *  Given a sequence of `as`, combine them and return the total.
   *
   *  If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  def tryProduct(as: TraversableOnce[A]): Option[A] = as.reduceOption(times)
}

trait MultiplicativeCSemigroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeSemigroup[A] {
  override def multiplicative: CSemigroup[A] = new CSemigroup[A] {
    def combine(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeMonoid[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeSemigroup[A] {
  override def multiplicative: Monoid[A] = new Monoid[A] {
    def empty: A = one
    def combine(x: A, y: A): A = times(x, y)
  }

  def one: A

  def isOne(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, one)

  /**
   * Return `a` multiplied with itself `n` times.
   */
  override def pow(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated multiplication for monoids must have reptitions >= 0")
    else if (n == 0) one
    else if (n == 1) a
    else positivePow(a, n)

  /**
   *  Given a sequence of `as`, sum them using the monoid and return the total.
   */
  def prod(as: TraversableOnce[A]): A = as.aggregate(one)(times, times)
}

trait MultiplicativeCMonoid[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeMonoid[A] with MultiplicativeCSemigroup[A] {
  override def multiplicative: CMonoid[A] = new CMonoid[A] {
    def empty: A = one
    def combine(x: A, y: A): A = times(x, y)
  }
}

trait MultiplicativeGroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeMonoid[A] {
  override def multiplicative: Group[A] = new Group[A] {
    def empty: A = one
    def combine(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }

  def reciprocal(x: A): A = div(one, x)
  def div(x: A, y: A): A

  /**
    * Return `a` multiplicated with itself `n` times.
    */
  override def pow(a: A, n: Int): A =
    if (n > 0) positivePow(a, n)
    else if (n == 0) one
    else if (n == Int.MinValue) positivePow(reciprocal(times(a, a)), 1073741824)
    else positivePow(reciprocal(a), -n)
}

trait MultiplicativeAbGroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with MultiplicativeGroup[A] with MultiplicativeCMonoid[A] {
  override def multiplicative: AbGroup[A] = new AbGroup[A] {
    def empty: A = one
    def combine(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }
}

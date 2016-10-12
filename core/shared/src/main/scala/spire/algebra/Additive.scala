package spire
package algebra



object Additive {
  def apply[A](s: Semigroup[A]): AdditiveSemigroup[A] = new AdditiveSemigroup[A] {
    def plus(x: A, y: A): A = s.combine(x, y)
  }

  def apply[A](s: CSemigroup[A]): AdditiveCSemigroup[A] = new AdditiveCSemigroup[A] {
    def plus(x: A, y: A): A = s.combine(x, y)
  }

  def apply[A](m: Monoid[A]): AdditiveMonoid[A] = new AdditiveMonoid[A] {
    def plus(x: A, y: A): A = m.combine(x, y)
    def zero: A = m.empty
  }

  def apply[A](m: CMonoid[A]): AdditiveCMonoid[A] = new AdditiveCMonoid[A] {
    def plus(x: A, y: A): A = m.combine(x, y)
    def zero: A = m.empty
  }

  def apply[A](g: Group[A]): AdditiveGroup[A] = new AdditiveGroup[A] {
    def plus(x: A, y: A): A = g.combine(x, y)
    override def minus(x: A, y: A): A = g.remove(x, y)
    def zero: A = g.empty
    def negate(x: A): A = g.inverse(x)
  }

  def apply[A](g: AbGroup[A]): AdditiveAbGroup[A] = new AdditiveAbGroup[A] {
    def plus(x: A, y: A): A = g.combine(x, y)
    override def minus(x: A, y: A): A = g.remove(x, y)
    def zero: A = g.empty
    def negate(x: A): A = g.inverse(x)
  }
}

trait AdditiveSemigroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def additive: Semigroup[A] = new Semigroup[A] {
    def combine(x: A, y: A): A = plus(x, y)
  }

  def plus(x: A, y: A): A


  /**
   * Return `a` added with itself `n` times.
   */
  def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else throw new IllegalArgumentException("Illegal non-positive exponent to sumN: %s" format n)

  protected[this] def positiveSumN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) plus(b, extra) else {
        val x = if ((k & 1) == 1) plus(b, extra) else extra
        loop(plus(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   *  Given a sequence of `as`, sum them using the semigroup and return the total.
   *
   *  If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  def trySum(as: TraversableOnce[A]): Option[A] = as.reduceOption(plus)
}

trait AdditiveCSemigroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveSemigroup[A] {
  override def additive: CSemigroup[A] = new CSemigroup[A] {
    def combine(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveMonoid[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveSemigroup[A] {
  override def additive: Monoid[A] = new Monoid[A] {
    def empty: A = zero
    def combine(x: A, y: A): A = plus(x, y)
  }

  def zero: A

  /**
    * Tests if `a` is zero.
    */
  def isZero(a: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, zero)

  /**
   * Return `a` added with itself `n` times.
   */
  override def sumN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated summation for monoids must have repetitions >= 0")
    else if (n == 0) zero
    else if (n == 1) a
    else positiveSumN(a, n)

  /**
   *  Given a sequence of `as`, sum them using the monoid and return the total.
   */
  def sum(as: TraversableOnce[A]): A = as.aggregate(zero)(plus, plus)
}

trait AdditiveCMonoid[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveMonoid[A] with AdditiveCSemigroup[A] {
  override def additive: CMonoid[A] = new CMonoid[A] {
    def empty: A = zero
    def combine(x: A, y: A): A = plus(x, y)
  }
}

trait AdditiveGroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveMonoid[A] {
  override def additive: Group[A] = new Group[A] {
    def empty: A = zero
    def combine(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))

  /**
   * Return `a` added with itself `n` times.
   */
  override def sumN(a: A, n: Int): A =
    if (n == Int.MinValue) plus(sumN(negate(a), Int.MaxValue), negate(a))
    else if (n < 0) sumN(negate(a), -n)
    else if (n == 0) zero
    else if (n == 1) a
    else positiveSumN(a, n)
}

trait AdditiveAbGroup[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with AdditiveGroup[A] with AdditiveCMonoid[A] {
  override def additive: AbGroup[A] = new AbGroup[A] {
    def empty: A = zero
    def combine(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }
}

object AdditiveGroup {
  @inline final def apply[A](implicit ev: AdditiveGroup[A]): AdditiveGroup[A] = ev
  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveGroup[A]`, this method returns a
   * `Group[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveGroup[A]): Group[A] = ev.additive
}

object AdditiveAbGroup {
  @inline final def apply[A](implicit ev: AdditiveAbGroup[A]): AdditiveAbGroup[A] = ev
  /**
   * This method converts an additive instance into a generic
   * instance.
   *
   * Given an implicit `AdditiveAbGroup[A]`, this method returns a
   * `AbGroup[A]`.
   */
  @inline final def additive[A](implicit ev: AdditiveAbGroup[A]): AbGroup[A] =
    ev.additive
}

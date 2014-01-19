package spire.math

/**
 * Approx[A, B] represents an ability to approximate As as Bs.
 * 
 * Successful calls to approximate() are expected to return an
 * "approximate" result. This is not yet precisely-defined, but at a
 * minimum if the given value exceed's B's range the approximation is
 * expected to fail.
 * 
 * Error is expected to return the absolute error of the approximation
 * or None, if an approximation is not possible.
 */
trait Approx[A, B] { self =>

  def approximate(a: A): Option[(B, Rational)]

  def canApproximate(a: A): Boolean =
    approximate(a).isDefined

  def compose[D](that: Approx[D, A]): Approx[D, B] =
    that andThen this

  def mapFrom[D](f: D => A): Approx[D, B] = new Approx[D, B] {
    def approximate(d: D): Option[(B, Rational)] = self.approximate(f(d))
  }

  def flatMapFrom[D](f: D => Option[A]): Approx[D, B] = new Approx[D, B] {
    def approximate(d: D): Option[(B, Rational)] = f(d).flatMap(self.approximate)
  }

  def andThen[C](that: Approx[B, C]): Approx[A, C] = new Approx[A, C] {
    def approximate(a: A): Option[(C, Rational)] =
      for {
        (b, e1) <- self.approximate(a)
        (c, e2) <- that.approximate(b)
      } yield (c, e1.abs + e2.abs)
  }

  def map[C](f: B => C): Approx[A, C] = new Approx[A, C] {
    def approximate(a: A): Option[(C, Rational)] =
      self.approximate(a).map { case (b, error) => (f(b), error) }
  }

  def flatMap[C](f: B => Option[C]): Approx[A, C] = new Approx[A, C] {
    def approximate(a: A): Option[(C, Rational)] =
      self.approximate(a).flatMap { case (b, error) => f(b).map(c => (c, error)) }
  }
}

/**
 * Coersion[A, B] represents an ability to convert some As to Bs.
 * 
 * Successful calls to coerce() are expected to exactly represent
 * the given value in B.
 * 
 * if canConvert(a1), and coerce(a1) = coerce(a2) then a1 = a2.
 */
trait Coersion[A, B] extends Approx[A, B] { self =>

  def coerce(a: A): Option[B]

  def canConvert(a: A): Boolean =
    coerce(a).isDefined

  def approximate(a: A): Option[(B, Rational)] =
    coerce(a).map(b => (b, Rational.zero))

  def convertOrSkip(as: Iterable[A]): Iterable[B] =
    as.flatMap(coerce)

  def convertOrDefault(as: Iterable[A])(default: B): Iterable[B] =
    as.map(a => coerce(a).getOrElse(default))

  def compose[D](that: Coersion[D, A]): Coersion[D, B] =
    that andThen this

  override def mapFrom[D](f: D => A): Coersion[D, B] = new Coersion[D, B] {
    def coerce(d: D): Option[B] = self.coerce(f(d))
  }

  override def flatMapFrom[D](f: D => Option[A]): Coersion[D, B] = new Coersion[D, B] {
    def coerce(d: D): Option[B] = f(d).flatMap(self.coerce)
  }

  def andThen[C](that: Coersion[B, C]): Coersion[A, C] = new Coersion[A, C] {
    def coerce(a: A): Option[C] = self.coerce(a).flatMap(that.coerce)
  }

  override def map[C](f: B => C): Coersion[A, C] = new Coersion[A, C] {
    def coerce(a: A): Option[C] = self.coerce(a).map(f)
  }

  override def flatMap[C](f: B => Option[C]): Coersion[A, C] = new Coersion[A, C] {
    def coerce(a: A): Option[C] = self.coerce(a).flatMap(f)
  }
}

/**
 * Conversion[A, B] represents an ability to convert all As to Bs.
 * 
 * Calls to convert() must always succeed, and are expected to exactly
 * represent the given value in B. This also means that an instance of
 * Conversion[A, B] provides an exact Approx[A, B].
 * 
 * if convert(a1) = convert(a2) then a1 = a2.
 * 
 * Conversion[A, B] is an injection from A to B.
 */
trait Conversion[A, B] extends Coersion[A, B] with Approx[A, B] { self =>

  def convert(a: A): B

  def coerce(a: A): Some[B] = Some(convert(a))

  override def canConvert(a: A): Boolean = true

  def convertAll(as: Iterable[A]): Iterable[B] =
    as.map(convert)

  override def convertOrSkip(as: Iterable[A]): Iterable[B] =
    convertAll(as)

  override def convertOrDefault(as: Iterable[A])(default: B): Iterable[B] =
    convertAll(as)

  def compose[D](that: Conversion[D, A]): Conversion[D, B] =
    that andThen this

  override def mapFrom[D](f: D => A): Conversion[D, B] = new Conversion[D, B] {
    def convert(d: D): B = self.convert(f(d))
  }

  def andThen[C](that: Conversion[B, C]): Conversion[A, C] = new Conversion[A, C] {
    def convert(a: A): C = that.convert(self.convert(a))
  }

  override def map[C](f: B => C): Conversion[A, C] = new Conversion[A, C] {
    def convert(a: A): C = f(self.convert(a))
  }

  //def approximate(a: A): Some[(B, Rational)] = Some((convert(a), Approx.NoError))
}

class ConversionOps[A](a: A) {
  def convert[B](implicit ev: Conversion[A, B]): B = ev.convert(a)
  def coerce[B](implicit ev: Coersion[A, B]): Option[B] = ev.coerce(a)
}

object Conversion {
  def apply[A, B](f: A => B): Conversion[A, B] = new Conversion[A, B] {
    def convert(a: A): B = f(a)
  }
}

object Coersion {
  def apply[A, B](f: A => Option[B]): Coersion[A, B] = new Coersion[A, B] {
    def coerce(a: A): Option[B] = f(a)
  }
}

object Approx {
  implicit def noop[A] = new Conversion[A, A] { def convert(a: A): A = a }

  def apply[A, B](f: A => Option[(B, Rational)]): Approx[A, B] = new Approx[A, B] {
    def approximate(a: A): Option[(B, Rational)] = f(a)
  }

  import scala.reflect.ClassTag
  import spire.algebra.{Eq, Order, Semiring, Signed, AdditiveMonoid}
  import spire.math.poly.Term
  import spire.syntax.std.seq._
  
  private[spire] def maybeList[A](mas: List[Option[A]]): Option[List[A]] = {
    def loop(acc: List[A], mas: List[Option[A]]): Option[List[A]] = mas match {
      case Nil => Some(acc.reverse)
      case None :: _ => None
      case Some(a) :: tail => loop(a :: acc, tail)
    }
    loop(Nil, mas)
  }

  // convert to Complex[_]
  implicit def RealToComplex1[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) = ev.map(b => Complex(b))
  implicit def RealToComplex2[A, B](implicit ev: Coersion[A, B], f: Semiring[B]) = ev.map(b => Complex(b))
  implicit def RealToComplex3[A, B](implicit ev: Approx[A, B], f: Semiring[B]) = ev.map(b => Complex(b))

  implicit def ComplexToComplex1[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    Conversion((a: Complex[A]) => Complex(ev.convert(a.real), ev.convert(a.imag)))
  implicit def ComplexToComplex2[A, B](implicit ev: Coersion[A, B], f: Semiring[B]) =
    Coersion((a: Complex[A]) => ev.coerce(a.real).flatMap(r => ev.coerce(a.imag).map(i => Complex(r, i))))
  implicit def ComplexToComplex3[A, B](implicit ev: Approx[A, B], f: Semiring[B]) =
    Approx((a: Complex[A]) => for {
      (r, e1) <- ev.approximate(a.real)
      (i, e2) <- ev.approximate(a.imag)
    } yield (Complex(r, i), e1.abs + e2.abs))

  implicit def QuaternionToComplex2[A, B](implicit ev: Coersion[A, B], f: Semiring[B], s: Signed[A]) =
    new Coersion[Quaternion[A], Complex[B]] {
      def coerce(a: Quaternion[A]): Option[Complex[B]] =
        if (s.signum(a.j) != 0 || s.signum(a.k) != 0) None
        else for {
          r <- ev.coerce(a.r)
          i <- ev.coerce(a.i)
        } yield Complex(r, i)
    }

  implicit def QuaternionToComplex3[A, B](implicit ev: Approx[A, B], f: Semiring[B], s: Signed[A]) =
    new Approx[Quaternion[A], Complex[B]] {
      def approximate(a: Quaternion[A]): Option[(Complex[B], Rational)] =
        if (s.signum(a.j) != 0 || s.signum(a.k) != 0) None
        else for {
          (r, e1) <- ev.approximate(a.r)
          (i, e2) <- ev.approximate(a.i)
        } yield (Complex(r, i), e1.abs + e2.abs)
    }
  
  // convert to Quaternion[_]
  implicit def RealToQuaternion1[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    ev.map(a => Quaternion(a))
  implicit def RealToQuaternion2[A, B](implicit ev: Coersion[A, B], f: Semiring[B]) =
    ev.map(a => Quaternion(a))
  implicit def RealToQuaternion3[A, B](implicit ev: Approx[A, B], f: Semiring[B]) =
    ev.map(a => Quaternion(a))

  implicit def ComplexToQuaternion1[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    Conversion((a: Complex[A]) => Quaternion(ev.convert(a.real), ev.convert(a.imag)))
  implicit def ComplexToQuaternion2[A, B](implicit ev: Coersion[A, B], f: Semiring[B]) =
    Coersion((a: Complex[A]) => ev.coerce(a.real).flatMap(r => ev.coerce(a.imag).map(i => Quaternion(r, i))))
  implicit def ComplexToQuaternion3[A, B](implicit ev: Approx[A, B], f: Semiring[B]) =
    Approx((a: Complex[A]) => for {
      (r, e1) <- ev.approximate(a.real)
      (i, e2) <- ev.approximate(a.imag)
    } yield (Quaternion(r, i), e1.abs + e2.abs))

  implicit def QuaternionToQuaternion1[A, B](implicit ev: Conversion[A, B], f: Semiring[B]) =
    Conversion((a: Quaternion[A]) => Quaternion(ev.convert(a.r), ev.convert(a.i), ev.convert(a.j), ev.convert(a.k)))
  implicit def QuaternionToQuaternion2[A, B](implicit ev: Coersion[A, B], f: Semiring[B]) =
    Coersion((a: Quaternion[A]) => for {
      r <- ev.coerce(a.r)
      i <- ev.coerce(a.i)
      j <- ev.coerce(a.j)
      k <- ev.coerce(a.k)
    } yield Quaternion(r, i, j, k))
  implicit def QuaternionToQuaternion3[A, B](implicit ev: Approx[A, B], f: Semiring[B]) =
    Approx((a: Quaternion[A]) => for {
      (r, e1) <- ev.approximate(a.r)
      (i, e2) <- ev.approximate(a.i)
      (j, e3) <- ev.approximate(a.j)
      (k, e4) <- ev.approximate(a.k)
    } yield (Quaternion(r, i, j, k), e1.abs + e2.abs + e3.abs + e4.abs))
  
  // convert to Interval[_]
  implicit def RealToInterval1[A, B](implicit ev: Conversion[A, B], o: Order[B]) = ev.map(b => Interval.point(b))
  implicit def RealToInterval2[A, B](implicit ev: Coersion[A, B], o: Order[B]) = ev.map(b => Interval.point(b))
  implicit def RealToInterval3[A, B](implicit ev: Approx[A, B], o: Order[B]) = ev.map(b => Interval.point(b))
  
  implicit def IntervalToInterval1[A, B](implicit ev: Conversion[A, B], f: AdditiveMonoid[B], o: Order[B]) =
    Conversion((a: Interval[A]) => a.mapBounds(ev.convert))
  implicit def IntervalToInterval2[A, B](implicit ev: Coersion[A, B], f: AdditiveMonoid[B], o: Order[B]) =
    Conversion((a: Interval[A]) => for {
      b1 <- Interval.Bound.lift(a.lowerBound.map(ev.coerce))
      b2 <- Interval.Bound.lift(a.upperBound.map(ev.coerce))
    } yield Interval.fromBounds(b1, b2))
  implicit def IntervalToInterval3[A, B](implicit ev: Approx[A, B], f: AdditiveMonoid[B], o: Order[B]) =
    Conversion((a: Interval[A]) => for {
      (b1, e1) <- Interval.Bound.break(a.lowerBound.map(ev.approximate))
      (b2, e2) <- Interval.Bound.break(a.upperBound.map(ev.approximate))
    } yield (Interval.fromBounds(b1, b2), e1.abs + e2.abs))
  
  // convert to Polynomial[_]
  implicit def RealToPolynomial1[A, B](implicit ev: Conversion[A, B], e: Eq[B], f: Semiring[B], ct: ClassTag[B]) =
    ev.map(b => Polynomial.constant(b))
  implicit def RealToPolynomial2[A, B](implicit ev: Coersion[A, B], e: Eq[B], f: Semiring[B], ct: ClassTag[B]) =
    ev.map(b => Polynomial.constant(b))
  implicit def RealToPolynomial3[A, B](implicit ev: Approx[A, B], e: Eq[B], f: Semiring[B], ct: ClassTag[B]) =
    ev.map(b => Polynomial.constant(b))
  
  implicit def PolynomialToPolynomial1[A, B](implicit ev: Conversion[A, B], ea: Eq[A], eb: Eq[B], fa: Semiring[A], fb: Semiring[B], ct: ClassTag[B]) =
    Conversion((a: Polynomial[A]) => Polynomial(a.terms.map { case Term(c, e) => Term(ev.convert(c), e) }))

  implicit def PolynomialToPolynomial2[A, B](implicit ev: Coersion[A, B], ea: Eq[A], eb: Eq[B], fa: Semiring[A], fb: Semiring[B], ct: ClassTag[B]) =
    Coersion((a: Polynomial[A]) => for {
      ts <- maybeList(a.terms.map { case Term(c, e) =>
        ev.coerce(c).map(b => Term(b, e))
      })
    } yield Polynomial(ts))

  implicit def PolynomialToPolynomial3[A, B](implicit ev: Approx[A, B], ea: Eq[A], eb: Eq[B], fa: Semiring[A], fb: Semiring[B], ct: ClassTag[B]) =
    Approx((a: Polynomial[A]) => for {
      (ts, errors) <- maybeList(a.terms.map { case Term(c, e) =>
        ev.approximate(c).map { case (b, error) => (Term(b, e), error) }
      }).map(_.unzip)
    } yield (Polynomial(ts), errors.map(_.abs).qsum))
  
  // UByte conversions
  implicit val UByteToUShort = new Conversion[UByte, UShort] { def convert(n: UByte): UShort = UShort(n.toInt) }
  implicit val UByteToUInt = new Conversion[UByte, UInt] { def convert(n: UByte): UInt = UInt(n.toInt) }
  implicit val UByteToULong = new Conversion[UByte, ULong] { def convert(n: UByte): ULong = ULong(n.toInt) }
  implicit val UByteToNatural = new Conversion[UByte, Natural] { def convert(n: UByte): Natural = Natural(n.toInt) }
  implicit val UByteToByte = Coersion((n: UByte) => if (n.signed >= 0) Some(n.toByte) else None)
  implicit val UByteToShort = new Conversion[UByte, Short] { def convert(n: UByte): Short = n.toShort }
  implicit val UByteToInt = new Conversion[UByte, Int] { def convert(n: UByte): Int = n.toInt }
  implicit val UByteToLong = new Conversion[UByte, Long] { def convert(n: UByte): Long = n.toLong }
  implicit val UByteToBigInt = new Conversion[UByte, BigInt] { def convert(n: UByte): BigInt = n.toBigInt }
  implicit val UByteToSafeLong = new Conversion[UByte, SafeLong] { def convert(n: UByte): SafeLong = SafeLong(n.toInt) }
  implicit val UByteToFloat = new Conversion[UByte, Float] { def convert(n: UByte): Float = n.toFloat }
  implicit val UByteToDouble = new Conversion[UByte, Double] { def convert(n: UByte): Double = n.toDouble }
  implicit val UByteToBigDecimal = new Conversion[UByte, BigDecimal] { def convert(n: UByte): BigDecimal = BigDecimal(n.toInt) }
  implicit val UByteToRational = Conversion((n: UByte) => Rational(n.toInt))
  implicit val UByteToAlgebraic = Conversion((n: UByte) => Algebraic(n.toInt))a
  implicit val UByteToReal = Conversion((n: UByte) => Real(n.toInt))
  
  // UShort conversions
  implicit val UShortToUByte = Coersion((n: UShort) => if (n < UShort(256)) Some(UByte(n.signed)) else None)
  implicit val UShortToUInt = new Conversion[UShort, UInt] { def convert(n: UShort): UInt = UInt(n.toInt) }
  implicit val UShortToULong = new Conversion[UShort, ULong] { def convert(n: UShort): ULong = ULong(n.toInt) }
  implicit val UShortToByte = Coersion((n: UShort) => if (n < UShort(128)) Some(n.toByte) else None)
  implicit val UShortToShort = Coersion((n: UShort) => if (n.signed < Short.MaxValue) Some(n.toShort) else None)
  implicit val UShortToNatural = new Conversion[UShort, Natural] { def convert(n: UShort): Natural = Natural(n.toInt) }
  implicit val UShortToInt = new Conversion[UShort, Int] { def convert(n: UShort): Int = n.toInt }
  implicit val UShortToLong = new Conversion[UShort, Long] { def convert(n: UShort): Long = n.toLong }
  implicit val UShortToBigInt = new Conversion[UShort, BigInt] { def convert(n: UShort): BigInt = n.toBigInt }
  implicit val UShortToSafeLong = new Conversion[UShort, SafeLong] { def convert(n: UShort): SafeLong = SafeLong(n.toInt) }
  implicit val UShortToFloat = new Conversion[UShort, Float] { def convert(n: UShort): Float = n.toFloat }
  implicit val UShortToDouble = new Conversion[UShort, Double] { def convert(n: UShort): Double = n.toDouble }
  implicit val UShortToBigDecimal = new Conversion[UShort, BigDecimal] { def convert(n: UShort): BigDecimal = BigDecimal(n.toInt) }
  implicit val UShortToRational = new Conversion[UShort, Rational] { def convert(n: UShort): Rational = Rational(n.toInt) }
  implicit val UShortToAlgebraic = new Conversion[UShort, Algebraic] { def convert(n: UShort): Algebraic = Algebraic(n.toInt) }
  implicit val UShortToReal = new Conversion[UShort, Real] { def convert(n: UShort): Real = Real(n.toInt) }
  
  // UInt conversions
  implicit val UIntToULong = new Conversion[UInt, ULong] { def convert(n: UInt): ULong = ULong(n.toLong) }
  implicit val UIntToNatural = new Conversion[UInt, Natural] { def convert(n: UInt): Natural = Natural(n.toLong) }
  implicit val UIntToShort = new Conversion[UInt, Short] { def convert(n: UInt): Short = n.toShort }
  implicit val UIntToLong = new Conversion[UInt, Long] { def convert(n: UInt): Long = n.toLong }
  implicit val UIntToBigInt = new Conversion[UInt, BigInt] { def convert(n: UInt): BigInt = n.toBigInt }
  implicit val UIntToSafeLong = new Conversion[UInt, SafeLong] { def convert(n: UInt): SafeLong = SafeLong(n.toLong) }
  implicit val UIntToDouble = new Conversion[UInt, Double] { def convert(n: UInt): Double = n.toDouble }
  implicit val UIntToBigDecimal = new Conversion[UInt, BigDecimal] { def convert(n: UInt): BigDecimal = BigDecimal(n.toLong) }
  implicit val UIntToRational = new Conversion[UInt, Rational] { def convert(n: UInt): Rational = Rational(n.toLong) }
  implicit val UIntToAlgebraic = new Conversion[UInt, Algebraic] { def convert(n: UInt): Algebraic = Algebraic(n.toLong) }
  implicit val UIntToReal = new Conversion[UInt, Real] { def convert(n: UInt): Real = Real(n.toLong) }
  
  // ULong conversions
  implicit val ULongToNatural = new Conversion[ULong, Natural] { def convert(n: ULong): Natural = n.toNatural }
  implicit val ULongToBigInt = new Conversion[ULong, BigInt] { def convert(n: ULong): BigInt = n.toBigInt }
  implicit val ULongToSafeLong = new Conversion[ULong, SafeLong] { def convert(n: ULong): SafeLong = n.toSafeLong }
  implicit val ULongToBigDecimal = new Conversion[ULong, BigDecimal] { def convert(n: ULong): BigDecimal = BigDecimal(n.toBigInt) }
  implicit val ULongToRational = new Conversion[ULong, Rational] { def convert(n: ULong): Rational = Rational(n.toSafeLong) }
  implicit val ULongToAlgebraic = new Conversion[ULong, Algebraic] { def convert(n: ULong): Algebraic = Algebraic(n.toSafeLong) }
  implicit val ULongToReal = new Conversion[ULong, Real] { def convert(n: ULong): Real = Real(n.toSafeLong) }
  
  // Natural conversions
  implicit val NaturalToBigInt = new Conversion[Natural, BigInt] { def convert(n: Natural): BigInt = n.toBigInt }
  implicit val NaturalToSafeLong = new Conversion[Natural, SafeLong] { def convert(n: Natural): SafeLong = SafeLong(n.toBigInt) }
  implicit val NaturalToBigDecimal = new Conversion[Natural, BigDecimal] { def convert(n: Natural): BigDecimal = BigDecimal(n.toBigInt) }
  implicit val NaturalToRational = new Conversion[Natural, Rational] { def convert(n: Natural): Rational = Rational(n.toBigInt) }
  implicit val NaturalToAlgebraic = new Conversion[Natural, Algebraic] { def convert(n: Natural): Algebraic = Algebraic(n.toBigInt) }
  implicit val NaturalToReal = new Conversion[Natural, Real] { def convert(n: Natural): Real = Real(n.toBigInt) }
  
  // Byte conversions
  implicit val ByteToShort = new Conversion[Byte, Short] { def convert(n: Byte): Short = n.toShort }
  implicit val ByteToInt = new Conversion[Byte, Int] { def convert(n: Byte): Int = n.toInt }
  implicit val ByteToLong = new Conversion[Byte, Long] { def convert(n: Byte): Long = n.toLong }
  implicit val ByteToBigInt = new Conversion[Byte, BigInt] { def convert(n: Byte): BigInt = BigInt(n) }
  implicit val ByteToSafeLong = new Conversion[Byte, SafeLong] { def convert(n: Byte): SafeLong = SafeLong(n) }
  implicit val ByteToFloat = new Conversion[Byte, Float] { def convert(n: Byte): Float = n.toFloat }
  implicit val ByteToDouble = new Conversion[Byte, Double] { def convert(n: Byte): Double = n.toDouble }
  implicit val ByteToBigDecimal = new Conversion[Byte, BigDecimal] { def convert(n: Byte): BigDecimal = BigDecimal(n) }
  implicit val ByteToRational = new Conversion[Byte, Rational] { def convert(n: Byte): Rational = Rational(n) }
  implicit val ByteToAlgebraic = new Conversion[Byte, Algebraic] { def convert(n: Byte): Algebraic = Algebraic(n) }
  implicit val ByteToReal = new Conversion[Byte, Real] { def convert(n: Byte): Real = Real(n) }
  
  // Short conversions
  implicit val ShortToInt = new Conversion[Short, Int] { def convert(n: Short): Int = n.toInt }
  implicit val ShortToLong = new Conversion[Short, Long] { def convert(n: Short): Long = n.toLong }
  implicit val ShortToBigInt = new Conversion[Short, BigInt] { def convert(n: Short): BigInt = BigInt(n) }
  implicit val ShortToSafeLong = new Conversion[Short, SafeLong] { def convert(n: Short): SafeLong = SafeLong(n) }
  implicit val ShortToFloat = new Conversion[Short, Float] { def convert(n: Short): Float = n.toFloat }
  implicit val ShortToDouble = new Conversion[Short, Double] { def convert(n: Short): Double = n.toDouble }
  implicit val ShortToBigDecimal = new Conversion[Short, BigDecimal] { def convert(n: Short): BigDecimal = BigDecimal(n) }
  implicit val ShortToRational = new Conversion[Short, Rational] { def convert(n: Short): Rational = Rational(n) }
  implicit val ShortToAlgebraic = new Conversion[Short, Algebraic] { def convert(n: Short): Algebraic = Algebraic(n) }
  implicit val ShortToReal = new Conversion[Short, Real] { def convert(n: Short): Real = Real(n) }
  
  // Int conversions
  implicit val IntToLong = new Conversion[Int, Long] { def convert(n: Int): Long = n.toLong }
  implicit val IntToBigInt = new Conversion[Int, BigInt] { def convert(n: Int): BigInt = BigInt(n) }
  implicit val IntToSafeLong = new Conversion[Int, SafeLong] { def convert(n: Int): SafeLong = SafeLong(n) }
  implicit val IntToDouble = new Conversion[Int, Double] { def convert(n: Int): Double = n.toDouble }
  implicit val IntToBigDecimal = new Conversion[Int, BigDecimal] { def convert(n: Int): BigDecimal = BigDecimal(n) }
  implicit val IntToRational = new Conversion[Int, Rational] { def convert(n: Int): Rational = Rational(n) }
  implicit val IntToAlgebraic = new Conversion[Int, Algebraic] { def convert(n: Int): Algebraic = Algebraic(n) }
  implicit val IntToReal = new Conversion[Int, Real] { def convert(n: Int): Real = Real(n) }
  
  // Long conversions
  implicit val LongToBigInt = new Conversion[Long, BigInt] { def convert(n: Long): BigInt = BigInt(n) }
  implicit val LongToSafeLong = new Conversion[Long, SafeLong] { def convert(n: Long): SafeLong = SafeLong(n) }
  implicit val LongToBigDecimal = new Conversion[Long, BigDecimal] { def convert(n: Long): BigDecimal = BigDecimal(n) }
  implicit val LongToRational = new Conversion[Long, Rational] { def convert(n: Long): Rational = Rational(n) }
  implicit val LongToAlgebraic = new Conversion[Long, Algebraic] { def convert(n: Long): Algebraic = Algebraic(n) }
  implicit val LongToReal = new Conversion[Long, Real] { def convert(n: Long): Real = Real(n) }
  
  // BigInt conversions
  implicit val BigIntToSafeLong = new Conversion[BigInt, SafeLong] { def convert(n: BigInt): SafeLong = SafeLong(n) }
  implicit val BigIntToBigDecimal = new Conversion[BigInt, BigDecimal] { def convert(n: BigInt): BigDecimal = BigDecimal(n) }
  implicit val BigIntToRational = new Conversion[BigInt, Rational] { def convert(n: BigInt): Rational = Rational(n) }
  implicit val BigIntToAlgebraic = new Conversion[BigInt, Algebraic] { def convert(n: BigInt): Algebraic = Algebraic(n) }
  implicit val BigIntToReal = new Conversion[BigInt, Real] { def convert(n: BigInt): Real = Real(n) }
  
  // SafeLong conversions
  implicit val SafeLongToBigInt = new Conversion[SafeLong, BigInt] { def convert(n: SafeLong): BigInt = n.toBigInt }
  implicit val SafeLongToBigDecimal = new Conversion[SafeLong, BigDecimal] { def convert(n: SafeLong): BigDecimal = BigDecimal(n.toBigInt) }
  implicit val SafeLongToRational = new Conversion[SafeLong, Rational] { def convert(n: SafeLong): Rational = Rational(n) }
  implicit val SafeLongToAlgebraic = new Conversion[SafeLong, Algebraic] { def convert(n: SafeLong): Algebraic = Algebraic(n) }
  implicit val SafeLongToReal = new Conversion[SafeLong, Real] { def convert(n: SafeLong): Real = Real(n) }
  
  // Float conversions
  implicit val FloatToDouble = new Conversion[Float, Double] { def convert(n: Float): Double = n.toDouble }
  implicit val FloatToBigDecimal = new Conversion[Float, BigDecimal] { def convert(n: Float): BigDecimal = BigDecimal(n) }
  implicit val FloatToRational = new Conversion[Float, Rational] { def convert(n: Float): Rational = Rational(n) }
  implicit val FloatToAlgebraic = new Conversion[Float, Algebraic] { def convert(n: Float): Algebraic = Algebraic(n) }
  implicit val FloatToReal = new Conversion[Float, Real] { def convert(n: Float): Real = Real(n) }
  
  // Double conversions
  implicit val DoubleToBigDecimal = new Conversion[Double, BigDecimal] { def convert(n: Double): BigDecimal = BigDecimal(n) }
  implicit val DoubleToRational = new Conversion[Double, Rational] { def convert(n: Double): Rational = Rational(n) }
  implicit val DoubleToAlgebraic = new Conversion[Double, Algebraic] { def convert(n: Double): Algebraic = Algebraic(n) }
  implicit val DoubleToReal = new Conversion[Double, Real] { def convert(n: Double): Real = Real(n) }
  
  // BigDecimal conversions
  implicit val BigDecimalToRational = new Conversion[BigDecimal, Rational] { def convert(n: BigDecimal): Rational = Rational(n) }
  implicit val BigDecimalToAlgebraic = new Conversion[BigDecimal, Algebraic] { def convert(n: BigDecimal): Algebraic = Algebraic(n) }
  implicit val BigDecimalToReal = new Conversion[BigDecimal, Real] { def convert(n: BigDecimal): Real = Real(n) }
  
  // Rational conversions
  implicit val RationalToAlgebraic = new Conversion[Rational, Algebraic] { def convert(n: Rational): Algebraic = Algebraic(n) }
  implicit val RationalToReal = new Conversion[Rational, Real] { def convert(n: Rational): Real = Real(n) }
}

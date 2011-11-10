package numerics.math


sealed trait Rat extends Ordered[Rat] {
  def inverse: Rat

  def +(rhs: Rat): Rat
  def -(rhs: Rat): Rat
  def *(rhs: Rat): Rat
  def /(rhs: Rat): Rat

  def +(rhs: LongRat): Rat
  def +(rhs: BigRat): Rat

  def -(rhs: LongRat): Rat
  def -(rhs: BigRat): Rat

  def *(rhs: LongRat): Rat
  def *(rhs: BigRat): Rat

  def /(rhs: LongRat): Rat
  def /(rhs: BigRat): Rat
}


object Rat {
  private[math] def apply(n: SafeLong, d: SafeLong): Rat = {
    n.foldWith[Rat,LongRat,BigRat](d)(LongRat(_, _), BigRat(_, _))
  }

  private[math] def gcd(a: Long, b: Long): Long = if (b == 0L) {
    a
  } else {
    gcd(b, a % b)
  }

  def apply(n: BigInt, d: BigInt): Rat = {
    val gcd = n.gcd(d)
    if (gcd == 1) {
      if (d < 0)
        Rat(SafeLong(-n), SafeLong(-d))
      else
        Rat(SafeLong(n), SafeLong(d))
    } else {
      if (d < 0)
        Rat(-SafeLong(n / gcd), -SafeLong(d / gcd))
      else
        Rat(SafeLong(n / gcd), SafeLong(d / gcd))
    }
  }

  def apply(n: Long, d: Long): Rat = {
    val gcd = Rat.gcd(n, d)
    if (gcd == 1L) {
      if (d < 0)
        Rat(SafeLong(-n), SafeLong(-d))
      else
        LongRat(n, d)
    } else {
      if (d < 0)
        LongRat(-n / gcd, -d / gcd)
      else
        LongRat(n / gcd, d / gcd)
    }
  }

   def canEqual(other: Any) = other.isInstanceOf[Rat]
}


case class LongRat private[math] (n: Long, d: Long) extends Rat {
  import Rat.gcd

  def inverse: Rat = if (n < 0L) LongRat(-d, -n) else LongRat(d, n)

  def +(r: LongRat): Rat = {
    val dgcd: Long = gcd(d, r.d)
    if (dgcd == 1L) {

      val num = SafeLong(n) * r.d + SafeLong(r.n) * d
      val den = SafeLong(d) * r.d
      Rat(num, den)

    } else {

      val lden: Long = d / dgcd
      val rden: Long = r.d / dgcd
      val num: SafeLong = SafeLong(n) * rden + SafeLong(r.n) * lden
      val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
      if (ngcd == 1L)
        Rat(num, SafeLong(lden) * r.d)
      else
        Rat(num / ngcd, SafeLong(lden) * (r.d / ngcd))
    }
  }

  def +(r: BigRat): Rat = {
    val dgcd: Long = gcd(d, (r.d % d).toLong)
    if (dgcd == 1L) {

      val num = SafeLong(r.d * n + r.n * d)
      val den = SafeLong(r.d * d)
      Rat(num, den)

    } else {

      val lden: Long = d / dgcd
      val rden: SafeLong = SafeLong(r.d) / dgcd
      val num: SafeLong = rden * n + SafeLong(r.n) * lden
      val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
      if (ngcd == 1L)
        Rat(num, SafeLong(lden) * r.d)
      else
        Rat(num / ngcd, SafeLong(r.d / ngcd) * lden)

    }
  }
  def +(rhs: Rat): Rat = rhs + this

  def -(r: LongRat): Rat = {
    val dgcd: Long = gcd(d, r.d)
    if (dgcd == 1L) {

      val num = SafeLong(n) * r.d - SafeLong(r.n) * d
      val den = SafeLong(d) * r.d
      Rat(num, den)

    } else {

      val lden: Long = d / dgcd
      val rden: Long = r.d / dgcd
      val num: SafeLong = SafeLong(n) * rden - SafeLong(r.n) * lden
      val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
      if (ngcd == 1L)
        Rat(num, SafeLong(lden) * r.d)
      else
        Rat(num / ngcd, SafeLong(lden) * (r.d / ngcd))
    }
  }

  def -(r: BigRat): Rat = {
    val dgcd: Long = gcd(d, (r.d % d).toLong)
    if (dgcd == 1L) {

      val num = SafeLong(r.d * n - r.n * d)
      val den = SafeLong(r.d * d)
      Rat(num, den)

    } else {

      val lden: Long = d / dgcd
      val rden: SafeLong = SafeLong(r.d) / dgcd
      val num: SafeLong = rden * n - SafeLong(r.n) * lden
      val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
      if (ngcd == 1L)
        Rat(num, SafeLong(lden) * r.d)
      else
        Rat(num / ngcd, SafeLong(r.d / ngcd) * lden)

    }
  }
  def -(rhs: Rat): Rat = rhs match {
    case that: LongRat => this - that
    case that: BigRat => this - that
  }


  def *(r: LongRat): Rat = {
    val a = gcd(n, r.d)
    val b = gcd(d, r.n)
    Rat(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
  }
  def *(r: BigRat): Rat = {
    val a = gcd(n, (r.d % n).toLong)
    val b = gcd(d, (r.n % d).toLong)
    Rat(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
  }
  def *(rhs: Rat): Rat = rhs * this


  def /(r: LongRat): Rat = {
    val a = gcd(n, r.n)
    val b = gcd(d, r.d)
    val num = SafeLong(n / a) * (r.d / b)
    val den = SafeLong(d / b) * (r.n / a)
    if (den < SafeLong.zero) Rat(-num, -den) else Rat(num, den)
  }
  def /(r: BigRat): Rat = {
    val a = gcd(n, (r.n % n).toLong)
    val b = gcd(d, (r.d % d).toLong)
    val num = SafeLong(n / a) * (r.d / b)
    val den = SafeLong(d / b) * (r.n / a)
    if (den < SafeLong.zero) Rat(-num, -den) else Rat(num, den)
  }
  def /(rhs: Rat): Rat = rhs match {
    case that: LongRat => this / that
    case that: BigRat => this / that
  }

  def compare(r: Rat): Int = r match {
    case r: LongRat => {
      val dgcd = gcd(d, r.d)
      if (dgcd == 1L)
        (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
      else
        (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
    }
    case r: BigRat => {
      val dgcd = gcd(d, (r.d % d).toLong)
      if (dgcd == 1L)
        (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
      else
        (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
    }
  }

  override def hashCode: Int = 29 * (37 * n.## + d.##)

  override def equals(that: Any): Boolean = that match {
    case that: LongRat => n == that.n && d == that.d
    case that: BigRat => false
    case _ => false
  }

  override def toString: String = "%d/%d" format (n, d)
}


case class BigRat private[math] (n: BigInt, d: BigInt) extends Rat {
  def inverse: Rat = if (n.signum < 0) BigRat(-d, -n) else BigRat(d, n)

  def +(rhs: LongRat): Rat = rhs + this
  def +(r: BigRat): Rat = {
    val dgcd: BigInt = d.gcd(r.d)
    if (dgcd == 1) {
      Rat(SafeLong(r.d * n + r.n * d), SafeLong(r.d * d))
    } else {
      val lden: BigInt = d / dgcd
      val rden: BigInt = r.d / dgcd
      val num: BigInt = rden * n + r.n * lden
      val ngcd: BigInt = num.gcd(dgcd)
      if (ngcd == 1)
        Rat(SafeLong(num), SafeLong(lden * r.d))
      else
        Rat(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
    }
  }
  def +(rhs: Rat): Rat = rhs + this


  def -(r: LongRat): Rat = {
    val dgcd: Long = Rat.gcd(r.d, (d % r.d).toLong)
    if (dgcd == 1L) {
      Rat(SafeLong(n * r.d - d * r.n), SafeLong(d * r.d))
    } else {
      val lden: SafeLong = SafeLong(d / dgcd)
      val rden: SafeLong = SafeLong(r.d / dgcd)
      val num: SafeLong = rden * n - lden * r.n
      val ngcd: Long = num.fold(Rat.gcd(dgcd, _), num => Rat.gcd(dgcd, (num % dgcd).toLong))
      if (ngcd == 1L)
        Rat(num, lden * r.d)
      else
        Rat(num / ngcd, lden * (r.d / ngcd))
    }
  }
  def -(r: BigRat): Rat = {
    val dgcd: BigInt = d.gcd(r.d)
    if (dgcd == 1) {
      Rat(SafeLong(r.d * n - r.n * d), SafeLong(r.d * d))
    } else {
      val lden: BigInt = d / dgcd
      val rden: BigInt = r.d / dgcd
      val num: BigInt = rden * n - r.n * lden
      val ngcd: BigInt = num.gcd(dgcd)
      if (ngcd == 1)
        Rat(SafeLong(num), SafeLong(lden * r.d))
      else
        Rat(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
    }
  }
  def -(rhs: Rat): Rat = rhs match {
    case that: LongRat => this - that
    case that: BigRat => this - that
  }


  def *(rhs: LongRat): Rat = rhs * this
  def *(r: BigRat): Rat = {
    val a = n.gcd(r.d)
    val b = d.gcd(r.n)
    Rat(SafeLong((n / a) * (r.n / b)), SafeLong((d / b) * (r.d / a)))
  }
  def *(r: Rat): Rat = r * this
  
  def /(r: LongRat): Rat = r.inverse * this
  def /(r: BigRat): Rat = {
    val a = n.gcd(r.n)
    val b = d.gcd(r.d)
    val num = SafeLong(n / a) * (r.d / b)
    val den = SafeLong(d / b) * (r.n / a)
    if (den < SafeLong.zero) Rat(-num, -den) else Rat(num, den)
  }
  def /(r: Rat): Rat = r.inverse * this

  def compare(r: Rat): Int = r match {
    case r: LongRat => {
      val dgcd = Rat.gcd(r.d, (d % r.d).toLong)
      if (dgcd == 1L)
        (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
      else
        (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
    }
    case r: BigRat => {
      val dgcd = d.gcd(r.d)
      if (dgcd == 1)
        (SafeLong(n * r.d) - r.n * d).signum
      else
        (SafeLong(r.d / dgcd) * n - SafeLong(d / dgcd) * r.n).signum
    }
  }

  override def hashCode: Int = 29 * (37 * n.## + d.##)

  override def equals(that: Any): Boolean = that match {
    case that: BigRat => n == that.n && d == that.d
    case that: LongRat => false
    case _ => false
  }


  override def toString: String = "%d/%d" format (n, d)
}

package spire
package benchmark
/*
import spire.implicits._

object LongRational {
  val Zero = new LongRational(0, 1)
  val One = new LongRational(1, 1)

  def apply(n: Long, d: Long): LongRational = {
    val div = gcd(n, d)
    if (d < 0) {
      new LongRational(-n / div, -d / div)
    } else {
      new LongRational(n / div, d / div)
    }
  }

  @inline final def gcd(a: Long, b: Long) = spire.math.gcd(a, b)
}

final class LongRational private (val n: Long, val d: Long) {
  import LongRational.gcd

  def unary_-(): LongRational = new LongRational(-n, d)

  def +(r: LongRational): LongRational = {
    val dgcd: Long = gcd(d, r.d)
    if (dgcd == 1) {
      new LongRational(r.d * n + r.n * d, r.d * d)
    } else {
      val lden: Long = d / dgcd
      val rden: Long = r.d / dgcd
      val num: Long = rden * n + r.n * lden
      val ngcd: Long = gcd(num, dgcd)
      if (ngcd == 1)
        new LongRational(num, lden * r.d)
      else
        new LongRational(num / ngcd, (r.d / ngcd) * lden)
    }
  }


  def -(r: LongRational): LongRational = {
    val dgcd: Long = gcd(d, r.d)
    if (dgcd == 1) {
      new LongRational(r.d * n - r.n * d, r.d * d)
    } else {
      val lden: Long = d / dgcd
      val rden: Long = r.d / dgcd
      val num: Long = rden * n - r.n * lden
      val ngcd: Long = gcd(num, dgcd)
      if (ngcd == 1)
        new LongRational(num, lden * r.d)
      else
        new LongRational(num / ngcd, (r.d / ngcd) * lden)
    }
  }


  def *(r: LongRational): LongRational = {
    val a = gcd(n, r.d)
    val b = gcd(d, r.n)
    new LongRational((n / a) * (r.n / b), (d / b) * (r.d / a))
  }


  def /(r: LongRational): LongRational = {
    val a = gcd(n, r.n)
    val b = gcd(d, r.d)
    val num = (n / a) * (r.d / b)
    val den = (d / b) * (r.n / a)
    if (den < 0L) {
      new LongRational(-num, -den)
    } else {
      new LongRational(num, den)
    }
  }

  def pow(exp: Int): LongRational = if (exp == 0) {
    LongRational.One
  } else if (exp < 0) {
    new LongRational(d pow java.lang.Math.abs(exp), n pow java.lang.Math.abs(exp))
  } else {
    new LongRational(n pow exp, d pow exp)
  }

  def compare(r: LongRational): Int = {
    val dgcd = gcd(d, r.d)
    if (dgcd == 1)
      java.lang.Math.signum(n * r.d - r.n * d).toInt
    else
      java.lang.Math.signum((r.d / dgcd) * n - (d / dgcd) * r.n).toInt
  }

  def signum: Int = if (n < 0) -1 else if (n > 0) 1 else 0
}


*/
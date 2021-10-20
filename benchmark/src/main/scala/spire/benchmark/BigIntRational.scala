/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package benchmark

object BigIntRational {
  val zero = new BigIntRational(0, 1)
  val one = new BigIntRational(1, 1)

  def apply(n: BigInt, d: BigInt): BigIntRational = {
    val gcd = n.gcd(d)
    if (d < 0) {
      new BigIntRational(-n / gcd, -d / gcd)
    } else {
      new BigIntRational(n / gcd, d / gcd)
    }
  }
}

final class BigIntRational private (val n: BigInt, val d: BigInt) {
  def unary_- : BigIntRational = new BigIntRational(-n, d)

  def +(r: BigIntRational): BigIntRational = {
    val dgcd: BigInt = d.gcd(r.d)
    if (dgcd == 1) {
      new BigIntRational(r.d * n + r.n * d, r.d * d)
    } else {
      val lden: BigInt = d / dgcd
      val rden: BigInt = r.d / dgcd
      val num: BigInt = rden * n + r.n * lden
      val ngcd: BigInt = num.gcd(dgcd)
      if (ngcd == 1)
        new BigIntRational(num, lden * r.d)
      else
        new BigIntRational(num / ngcd, (r.d / ngcd) * lden)
    }
  }

  def -(r: BigIntRational): BigIntRational = {
    val dgcd: BigInt = d.gcd(r.d)
    if (dgcd == 1) {
      new BigIntRational(r.d * n - r.n * d, r.d * d)
    } else {
      val lden: BigInt = d / dgcd
      val rden: BigInt = r.d / dgcd
      val num: BigInt = rden * n - r.n * lden
      val ngcd: BigInt = num.gcd(dgcd)
      if (ngcd == 1)
        new BigIntRational(num, lden * r.d)
      else
        new BigIntRational(num / ngcd, (r.d / ngcd) * lden)
    }
  }

  def *(r: BigIntRational): BigIntRational = {
    val a = n.gcd(r.d)
    val b = d.gcd(r.n)
    new BigIntRational((n / a) * (r.n / b), (d / b) * (r.d / a))
  }

  def /(r: BigIntRational): BigIntRational = {
    val a = n.gcd(r.n)
    val b = d.gcd(r.d)
    val num = (n / a) * (r.d / b)
    val den = (d / b) * (r.n / a)
    if (den < BigInt(0)) {
      new BigIntRational(-num, -den)
    } else {
      new BigIntRational(num, den)
    }
  }

  def pow(exp: Int): BigIntRational = if (exp == 0) {
    BigIntRational.one
  } else if (exp < 0) {
    new BigIntRational(d.pow(exp.abs), n.pow(exp.abs))
  } else {
    new BigIntRational(n.pow(exp), d.pow(exp))
  }

  def compare(r: BigIntRational): Int = {
    val dgcd = d.gcd(r.d)
    if (dgcd == 1)
      (n * r.d - r.n * d).signum
    else
      ((r.d / dgcd) * n - (d / dgcd) * r.n).signum
  }

  def signum: Int = n.signum

}

package spire
package math
package poly

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.Set
import scala.reflect.ClassTag
import scala.{specialized => spec}

import spire.algebra._
import spire.math._
import spire.syntax.cfor._
import spire.syntax.field._

/** Multivariate polynomial.
  * 
  * Properties:
  * 
  * - the monomials are sorted according to the implicit order given,
  * - all coefficients are non-zero.
  */
final class MultivariatePolynomial[@spec(Double, Long) C:ClassTag:CRing:Eq] private[spire] (private[spire] val _c: Array[C], private[spire] val _m: Array[Monomial])(implicit val order: Order[Monomial]) { lhs =>

  /** Number of terms in the polynomial. */
  def nTerms: Int = _c.length

  /** Coefficient of the i-th term. */
  def coefficient(i: Int): C = _c(i)

  /** Monomial for the i-th term. */
  def monomial(i: Int): Monomial = _m(i)

  def isZero: Boolean = nTerms == 0

  def isConstant: Boolean = (nTerms == 1) && monomial(0).isEmpty

  /** Polynomial degree. */
  def degree: Int = {
    var res = 0
    cforRange(0 until nTerms) { i =>
      res = max(res, monomial(i).degree)
    }
    res
  }

  /** Returns an array containing all variables. */
  def allVariables: Array[Char] = {
    val bitset = scala.collection.mutable.BitSet.empty
    cforRange(0 until nTerms) { i =>
      cforRange(0 until monomial(i).nVariables) { j =>
        bitset += monomial(i).variable(j)
      }
    }
    val res = new Array[Char](bitset.size)
    var i = 0
    bitset.foreach { v =>
      res(i) = v.toChar
      i += 1
    }
    res
  }

  /** Returns a map of all the terms (monomials and associated coefficients). */
  def allTerms: Map[Monomial, C] = Map(_m zip _c: _*)

  /** Evaluates the polynomial for the given variable values. `f` must be defined
    * on the variables of this polynomial.
    */
  def eval(f: Char => C): C = {
    var res = CRing[C].zero
    cforRange(0 until nTerms) { i =>
      res = res + coefficient(i) * monomial(i).eval(f)
    }
    res
  }

  /** Substitutes the variables for which `f` is defined. */
  def evalPartial(f: PartialFunction[Char, C]): MultivariatePolynomial[C] = {
    var n = 0
    var hasEmpty = false
    var constant = CRing[C].zero
    val nonEmpty = scala.collection.mutable.BitSet.empty
    cforRange(0 until nTerms) { i =>
      val mono = monomial(i)
      val nu = mono.nUndefinedVariables(f)
      if (nu == 0) {
        if (mono.isEmpty) {
          hasEmpty = true
          constant += coefficient(i)
        } else
          constant += coefficient(i) * mono.eval(f)
      } else {
        nonEmpty += i
        n += 1
      }
    }
    val zeroConstant = constant.isZero
    if (!zeroConstant && !hasEmpty) n += 1
    val newM: Array[Monomial] = new Array[Monomial](n)
    val newC: Array[C] = new Array[C](n)
    var ni = 0
    if (!zeroConstant) {
      newM(0) = Monomial.empty
      newC(0) = constant
      ni += 1
    }
    nonEmpty.foreach { i =>
      val (resC, resM) = monomial(i).evalPartial(f)
      newM(ni) = resM
      newC(ni) = resC
      ni += 1
    }
    MultivariatePolynomial.sort(newM, newC)(order)
    new MultivariatePolynomial(newC, newM)
  }

  def unary_- : MultivariatePolynomial[C] = {
    val newC = new Array[C](nTerms)
    cforRange(0 until nTerms) { i => newC(i) = -coefficient(i) }
    new MultivariatePolynomial[C](newC, _m)
  }

  def :*(rhs: C) =
    if (rhs.isZero) MultivariatePolynomial.zero[C]
    else if (CRing[C].isOne(rhs)) lhs
    else {
      val newC = new Array[C](nTerms)
      cforRange(0 until nTerms) { i => newC(i) = coefficient(i) * rhs }
      new MultivariatePolynomial[C](newC, _m)
    }

  def *:(realLhs: C) = lhs :* realLhs

  def :/(rhs: C)(implicit f: Field[C]) = if (CRing[C].isOne(rhs)) lhs else lhs :* (rhs.reciprocal)

  def monic(implicit f: Field[C]): MultivariatePolynomial[C] =
    if (isZero) lhs else lhs :/ coefficient(0)

  def +(rhs: MultivariatePolynomial[C]): MultivariatePolynomial[C] =
    if (rhs.isZero) lhs
    else if (lhs.isZero) rhs
    else if (rhs.nTerms > lhs.nTerms) rhs + lhs
    else {
      val map = scala.collection.mutable.HashMap.empty[Monomial, C]
      cforRange(0 until lhs.nTerms) { i => map(lhs.monomial(i)) = lhs.coefficient(i) }
      cforRange(0 until rhs.nTerms) { i =>
        val mono = rhs.monomial(i)
        if (map.isDefinedAt(mono))
          map(mono) = map(mono) + rhs.coefficient(i)
        else
          map(mono) = rhs.coefficient(i)
      }
      map.retain { (mono, coeff) => !coeff.isZero }
      MultivariatePolynomial.fromFilteredMap(map)
    }

  def -(rhs: MultivariatePolynomial[C]): MultivariatePolynomial[C] = lhs + (-rhs)

  def *(rhs: MultivariatePolynomial[C]): MultivariatePolynomial[C] =
    if (rhs.isZero) MultivariatePolynomial.zero[C]
    else if (rhs.isConstant) lhs :* rhs.coefficient(0)
    else if (rhs.nTerms > lhs.nTerms) rhs * lhs
    else {
      val map = scala.collection.mutable.HashMap.empty[Monomial, C]
      cforRange(0 until lhs.nTerms) { li =>
        cforRange(0 until rhs.nTerms) { ri =>
          val nm = lhs.monomial(li) * rhs.monomial(ri)
          val nc = lhs.coefficient(li) * rhs.coefficient(ri)
          if (map.isDefinedAt(nm))
            map(nm) = map(nm) + nc
          else
            map(nm) = nc
        }
      }
      map.retain { (mono, coeff) => !coeff.isZero }
      MultivariatePolynomial.fromFilteredMap(map)
    }

  def sortedBy(newOrder: Order[Monomial]): MultivariatePolynomial[C] = {
    val newM = _m.clone
    val newC = _c.clone
    MultivariatePolynomial.sort(newM, newC)(newOrder)
    new MultivariatePolynomial(newC, newM)
  }

}

object MultivariatePolynomial {

  protected[spire] def fromFilteredMap[C:ClassTag:CRing:Eq](map: scala.collection.Map[Monomial, C])(implicit order: Order[Monomial]): MultivariatePolynomial[C] = {
    val newM: Array[Monomial] = map.keys.toArray
    Sorting.sort(newM)(order, implicitly)
    val newC: Array[C] = new Array[C](newM.length)
    cforRange(0 until newM.length) { ni =>
      newC(ni) = map(newM(ni))
    }
    new MultivariatePolynomial[C](newC, newM)
  }

  def zero[C:ClassTag:CRing:Eq](implicit order: Order[Monomial]): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](Array.empty[C], Array.empty[Monomial])

  def apply[C:ClassTag:CRing:Eq](map: Map[Monomial, C])(implicit order: Order[Monomial]): MultivariatePolynomial[C] = {
    fromFilteredMap(map.filterNot(_._2.isZero))
  }

  final def sort[A, @spec B](data1: Array[A], data2: Array[B])(implicit o: Order[A]): Unit = {
    val end = data1.length
    var i = 1
    while (i < end) {
      val item1 = data1(i)
      val item2 = data2(i)
      var hole = i
      while (hole > 0 && o.gt(data1(hole - 1), item1)) {
        data1(hole) = data1(hole - 1)
        data2(hole) = data2(hole - 1)
        hole -= 1
      }
      data1(hole) = item1
      data2(hole) = item2
      i += 1
    }
  }

}

/*
class MultivariatePolynomialEuclideanRing[C](implicit val order: Order[Monomial]) {
  def /~(rhs: MultivariatePolynomial[C])(implicit f: Field[C], ct: ClassTag[C]): MultivariatePolynomial[C] =
    lhs./%(rhs)._1

  def %(rhs: MultivariatePolynomial[C])(implicit f: Field[C], ct: ClassTag[C]): MultivariatePolynomial[C] =
    lhs./%(rhs)._2

  def /%(rhs: MultivariatePolynomial[C])(implicit f: Field[C], ct: ClassTag[C]) = {

    @tailrec def quotMod_(quot: MultivariatePolynomial[C],
                          dividend: MultivariatePolynomial[C],
                          divisor: MultivariatePolynomial[C]): (MultivariatePolynomial[C], MultivariatePolynomial[C]) = {
      if(divisor.isEmpty || dividend.isEmpty) (quot, dividend) else {
        if(divisor.head.divides(dividend.head)) {
          val divTerm = MultivariatePolynomial[C](dividend.head / divisor.head)
          val prod = divisor * divTerm
          val quotSum = quot + divTerm
          val rem = dividend - prod
          if(rem.isZero) (quotSum, rem) else quotMod_(quotSum, rem, divisor)
        } else if(!rhs.allTerms.forall(t => t.divides(dividend.head))) (quot, dividend) else quotMod_(quot, dividend, divisor.tail)
      }
    }

    if (lhs == rhs) {
      (MultivariatePolynomial.one[C], MultivariatePolynomial.zero[C])
    } else if (rhs == MultivariatePolynomial.one[C]) {
      (lhs, MultivariatePolynomial.zero[C])
    } else if (rhs == MultivariatePolynomial.zero[C]) {
      (lhs, MultivariatePolynomial.zero[C])
    } else if (!rhs.head.divides(lhs.head)) {
      (MultivariatePolynomial.zero[C], lhs)
    } else {
      quotMod_(MultivariatePolynomial.zero[C], lhs, rhs)
    }
  }

}
 */

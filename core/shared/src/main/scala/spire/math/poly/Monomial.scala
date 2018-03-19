package spire
package math
package poly

import scala.annotation.tailrec

import spire.algebra._
import spire.math._
import spire.std.char._
import spire.syntax.cfor._
import spire.syntax.multiplicativeMonoid._
import spire.util.Opt

/** Monomial.
  * 
  * Properties:
  * 
  * - variables are unique and sorted,
  * - all variables have exponent > 0.
  * 
  * Note: the arrays _v and _e are wrapped and have to be considered immutable, as they can
  * be shared between monomials.
  * 
  */
final class Monomial private[spire] (private[spire] val _v: Array[Char], private[spire] val _e: Array[Int]) { lhs =>

  /* Future optimizations:
   *
   * - replace `_v: Array[Char]` by `_v: Array[Byte]` with a suitable encoding, or
   * - replace both `_v` and `_e` by a single array to reduce the number of allocations.
   */

  /** Number of variables involved in this monomial. */
  @inline def nVariables: Int = _v.length

  /** Returns the index of the provided variable, or `-insertionIndex-1`. */
  @inline def find(variable: Char): Int = Searching.search(_v, variable)

  /** Symbol of the i-th variable. */
  @inline def variable(i: Int): Char = _v(i)

  /** Exponent of the i-th variable. By construction > 0. */
  @inline def exponent(i: Int): Int = _e(i)

  /** Returns the exponent corresponding to the given variable or throws. */
  def exponent(variable: Char): Int = {
    val i = find(variable)
    if (i >= 0)
      exponent(i)
    else
      throw new Exception(s"Variable $variable is not contained in this monomial")
  }

  /** Count the number of unique variables in `lhs` and `rhs`. */
  def nVariablesInTotal(rhs: Monomial): Int = {
    @tailrec def count(nUnique: Int, li: Int, ri: Int): Int =
      if (li == lhs.nVariables) nUnique + rhs.nVariables - ri
      else if (ri == rhs.nVariables) nUnique + lhs.nVariables - li
      else {
        val lv = lhs.variable(li)
        val rv = rhs.variable(ri)
        if (lv == rv)
          count(nUnique + 1, li + 1, ri + 1)
        else if (lv < rv)
          count(nUnique + 1, li + 1, ri)
        else
          count(nUnique + 1, li, ri + 1)
      }
    count(0, 0, 0)
  }

  /** Count the number of common variables between `lhs` and `rhs`. */
  def nVariablesInCommon(rhs: Monomial): Int = {
    @tailrec def count(nCommon: Int, li: Int, ri: Int): Int =
      if (li == lhs.nVariables) nCommon
      else if (ri == rhs.nVariables) nCommon
      else {
        val lv = lhs.variable(li)
        val rv = rhs.variable(ri)
        if (lv == rv)
          count(nCommon + 1, li + 1, ri + 1)
        else if (lv < rv)
          count(nCommon, li + 1, ri)
        else
          count(nCommon, li, ri + 1)
      }
    count(0, 0, 0)
  }

  /** Degree of this monomial.
    * 
    * Cached because of its frequent use in monomial orderings.
    */
  lazy val degree = {
    var res = 0
    cforRange(0 until nVariables) { i => res += _e(i) }
    res
  }

  override def toString = {
    val sb = new StringBuilder
    cforRange(0 until nVariables) { i =>
      val v = variable(i)
      val e = exponent(i)
      sb += v
      if (e > 1) {
        sb += '^'
        sb ++= e.toString
      }
    }
    sb.toString
  }

  /** Cached hash value. */
  protected lazy val hash = { // taken from MurmurHash3.unorderedHashing
    import scala.util.hashing.MurmurHash3.{mix, finalizeHash}
    var h = 0xe73a8b15
    cforRange(0 until nVariables) { i =>
      h = mix(h, (variable(i) << 16) + exponent(i))
    }
    finalizeHash(h, nVariables)
  }

  override def hashCode = hash

  override def equals(that: Any): Boolean = that match {
    case rhs: Monomial =>
      (lhs.nVariables == rhs.nVariables) &&
      (lhs.hashCode == rhs.hashCode) && {
        cforRange(0 until lhs.nVariables) { i =>
          if (lhs.variable(i) != rhs.variable(i) || lhs.exponent(i) != rhs.exponent(i))
            return false
        }
        true
      }
    case _ => false // TODO: canEqual, cooperative equality with MultivariatePolynomial ???
  }

  /** Is this monomial one (i.e. empty) ? */
  def isOne = nVariables == 0

  /** Returns the value of this monomial, when the values associated to the variables
    * are given by the function `f`. 
    * 
    * By design, the variables are assumed to be commutative.
    */
  def eval[@sp(Double, Long) C](f: Char => C)(implicit C: MultiplicativeCMonoid[C]): C = {
    var res = C.one
    cforRange(0 until nVariables) { i =>
      res = res * C.prodn(f(variable(i)), exponent(i))
    }
    res
  }

  def nUndefinedVariables(f: PartialFunction[Char, _]): Int = {
    var nUndefined = 0
    cforRange(0 until nVariables) { i =>
      if (!f.isDefinedAt(variable(i)))
        nUndefined += 1
    }
    nUndefined
  }

  /** Substitutes the variables defined by the partial function `f` and returns the resulting
   * coefficient and monomial. */
  def evalPartial[@sp(Double, Long) C](f: PartialFunction[Char, C])(implicit C: MultiplicativeCMonoid[C]): (C, Monomial) = {
    val nUndefined = nUndefinedVariables(f)
    if (nUndefined == 0)
      (eval(f), Monomial.one)
    else {
      val newV = new Array[Char](nUndefined)
      val newE = new Array[Int](nUndefined)
      var ni = 0
      var res = C.one
      cforRange(0 until nVariables) { i =>
        val v = variable(i)
        val e = exponent(i)
        if (f.isDefinedAt(v))
          res = res * C.prodn(f(v), e)
        else {
          newV(ni) = v
          newE(ni) = e
          ni += 1
        }
      }
      require(ni == nUndefined)
      (res, new Monomial(newV, newE))
    }
  }

  /** Product with variable and possible exponent. */
  def *(variable: Char, exponent: Int = 1): Monomial =
    if (exponent == 0) lhs else {
      require(exponent > 0)
      val ind = find(variable)
      if (ind >= 0) {
        val newV = _v.clone
        val newE = _e.clone
        newE(ind) += exponent
        new Monomial(newV, newE)
      } else {
        val newV = new Array[Char](nVariables + 1)
        val newE = new Array[Int](nVariables + 1)
        val ins = -ind-1
        Array.copy(_v, 0, newV, 0, ins)
        Array.copy(_e, 0, newE, 0, ins)
        newV(ins) = variable
        newE(ins) = exponent
        Array.copy(_v, ins, newV, ins + 1, nVariables - ins)
        Array.copy(_e, ins, newE, ins + 1, nVariables - ins)
        new Monomial(newV, newE)
      }
    }

  /** Multiplication by monomial. */
  def *(rhs: Monomial): Monomial = {
    val n = lhs.nVariablesInTotal(rhs)
    val newV = new Array[Char](n)
    val newE = new Array[Int](n)
    @tailrec def rec(ni: Int, li: Int, ri: Int): Unit =
      if (li == lhs.nVariables && ri == rhs.nVariables) {
        // we are done
      } else if (li == lhs.nVariables) {
        // copy the remaining variables, we are done
        Array.copy(rhs._v, ri, newV, ni, rhs.nVariables - ri)
        Array.copy(rhs._e, ri, newE, ni, rhs.nVariables - ri)
      } else if (ri == rhs.nVariables) {
        // copy the remaining variables, we are done
        Array.copy(lhs._v, li, newV, ni, lhs.nVariables - li)
        Array.copy(lhs._e, li, newE, ni, lhs.nVariables - li)
      } else {
        val lv = lhs.variable(li)
        val rv = rhs.variable(ri)
        if (lv == rv) {
          newV(ni) = lv
          newE(ni) = lhs.exponent(li) + rhs.exponent(ri)
          rec(ni + 1, li + 1, ri + 1)
        } else if (lv < rv) {
          newV(ni) = lv
          newE(ni) = lhs.exponent(li)
          rec(ni + 1, li + 1, ri)
        } else { // lv > rv
          newV(ni) = rv
          newE(ni) = rhs.exponent(ri)
          rec(ni + 1, li, ri + 1)
        }
      }
    rec(0, 0, 0)
    new Monomial(newV, newE)
  }

  /** Power. */
  def pow(rhs: Int): Monomial =
    if (rhs == 0) Monomial.one
    else if (rhs == 1) lhs
    else {
      require(rhs >= 0)
      val newE = new Array[Int](nVariables)
      cforRange(0 until nVariables) { i =>
        newE(i) = exponent(i) * rhs
      }
      new Monomial(_v, newE) // reusing the variable array, which we safely wrapped
    }


  /** Least common multiple. */
  def lcm(rhs: Monomial): Monomial =
    if (lhs.isOne) rhs
    else if (rhs.isOne) lhs
    else {
      val n = lhs.nVariablesInTotal(rhs)
      val newV = new Array[Char](n)
      val newE = new Array[Int](n)
      @tailrec def rec(ni: Int, li: Int, ri: Int): Unit =
        if (li == lhs.nVariables && ri == rhs.nVariables) {
          // we are done
        } else if (li == lhs.nVariables) {
          // copy the remaining variables, we are done
          Array.copy(rhs._v, ri, newV, ni, rhs.nVariables - ri)
          Array.copy(rhs._e, ri, newE, ni, rhs.nVariables - ri)
        } else if (ri == rhs.nVariables) {
          // copy the remaining variables, we are done
          Array.copy(lhs._v, li, newV, ni, lhs.nVariables - li)
          Array.copy(lhs._e, li, newE, ni, lhs.nVariables - li)
        } else {
          val lv = lhs.variable(li)
          val rv = rhs.variable(ri)
          if (lv == rv) {
            newV(ni) = lv
            newE(ni) = max(lhs.exponent(li), rhs.exponent(ri))
            rec(ni + 1, li + 1, ri + 1)
          } else if (lv < rv) {
            newV(ni) = lv
            newE(ni) = lhs.exponent(li)
            rec(ni + 1, li + 1, ri)
          } else { // lv > rv
            newV(ni) = rv
            newE(ni) = rhs.exponent(ri)
            rec(ni + 1, li, ri + 1)
          }
        }
      rec(0, 0, 0)
      new Monomial(newV, newE)
    }

  /** Greatest common factor. */
  def gcf(rhs: Monomial): Monomial = {
    val n = lhs.nVariablesInCommon(rhs)
    val newV = new Array[Char](n)
    val newE = new Array[Int](n)
    @tailrec def rec(ni: Int, li: Int, ri: Int): Unit =
      if (li == lhs.nVariables || ri == rhs.nVariables) {
        // we are done
      } else {
        val lv = lhs.variable(li)
        val rv = rhs.variable(ri)
        if (lv == rv) {
          newV(ni) = lv
          newE(ni) = min(lhs.exponent(li), rhs.exponent(ri))
          rec(ni + 1, li + 1, ri + 1)
        } else if (lv < rv)
          rec(ni, li + 1, ri)
        else // lv > rv
          rec(ni, li, ri + 1)
      }
    rec(0, 0, 0)
    new Monomial(newV, newE)
  }

  /** Returns whether there is a monomial `x` such that `lhs * x == rhs`. */
  def divides(rhs: Monomial): Boolean = {
    @tailrec def test(li: Int, ri: Int): Boolean =
      if (li == lhs.nVariables) true // we could pass variables, and now the rest of `lhs` is empty
      else if (ri == rhs.nVariables) false // after passing common variables, `rhs` is empty, but `lhs` is not
      else {
        val lv = lhs.variable(li)
        val rv = rhs.variable(ri)
        if (lv < rv)
          false // there is a variable in `lhs` not present in `rhs`
        else if (lv > rv)
          test(li, ri + 1) // there is a variable is `rhs` not present in `lhs`, ok
        else { // lv == rv, common variable
          val le = lhs.exponent(li)
          val re = rhs.exponent(ri)
          if (le > re)
            false // there is more of this variable in `lhs`
          else
            test(li + 1, ri + 1) // move on, the exponents are compatible
        }
      }
    test(0, 0)
  }

  /** If `rhs.divides(lhs)`, returns `x` such that `rhs * x == lhs`. */
  def /(rhs: Monomial): Opt[Monomial] =
    if (rhs.isOne) Opt(lhs) else {
      /** Counts the number of variables in the returned monomial. */
      @tailrec def countNum(acc: Int, li: Int, ri: Int): Int =
        if (ri == rhs.nVariables) acc + lhs.nVariables - li
        else if (li == lhs.nVariables) -1 // rhs does not divides lhs
        else {
          val lv = lhs.variable(li)
          val rv = rhs.variable(ri)
          if (lv > rv) -1 // there is a variable in `rhs` not present in `lhs`
          else if (lv < rv) countNum(acc + 1, li + 1, ri) // variable in `lhs` not present in `rhs`
          else { // lv == rv, common variable
            require(lv == rv)
            val le = lhs.exponent(li)
            val re = rhs.exponent(ri)
            if (le < re) -1 // there is more of this variable in `rhs`
            else if (le > re) countNum(acc + 1, li + 1, ri + 1)
            else countNum(acc, li + 1, ri + 1) // le == re, the variable will be removed
          }
        }
      val n = countNum(0, 0, 0)
      if (n == -1)
        Opt.empty[Monomial]
      else if (n == 0)
        Opt(Monomial.one)
      else {
        val newV = new Array[Char](n)
        val newE = new Array[Int](n)
        @tailrec def rec(ni: Int, li: Int, ri: Int): Unit =
          if (ri == rhs.nVariables) {
            Array.copy(lhs._v, li, newV, ni, lhs.nVariables - li)
            Array.copy(lhs._e, li, newE, ni, lhs.nVariables - li)
          } else if (li == lhs.nVariables) sys.error("Already tested")
          else {
            val lv = lhs.variable(li)
            val rv = rhs.variable(ri)
            if (lv > rv) sys.error("Already tested")
            else if (lv < rv) {
              newV(ni) = lv // variable in `lhs` not present in `rhs`
              newE(ni) = lhs.exponent(li)
              rec(ni + 1, li + 1, ri)
            } else { // lv == rv, common variable
              val le = lhs.exponent(li)
              val re = rhs.exponent(ri)
              if (le < re) sys.error("Already tested")
              else if (le > re) {
                newV(ni) = lv // variable in `lhs` and `rhs` with a greater exponent in `lhs`
                newE(ni) = lhs.exponent(li) - rhs.exponent(ri)
                rec(ni + 1, li + 1, ri + 1)
              } else // le == re, the variable is removed
                rec(ni, li + 1, ri + 1)
            }
          }
        rec(0, 0, 0)
        Opt(new Monomial(newV, newE))
      }
    }

}

object Monomial {

  val one = new Monomial(Array.empty[Char], Array.empty[Int])

  def apply(variable: Char): Monomial = new Monomial(Array(variable), Array(1))

  def apply(variable: Char, exponent: Int): Monomial =
    if (exponent < 0) throw new Exception("Negative exponents are not allowed")
    else if (exponent == 0) one
    else new Monomial(Array(variable), Array(exponent))

  protected[spire] def fromFilteredMap(map: scala.collection.Map[Char, Int]): Monomial = {
    val newV: Array[Char] = map.keys.toArray
    Sorting.sort(newV)
    val newE: Array[Int] = new Array[Int](newV.length)
    cforRange(0 until newV.length) { ni =>
      val v = newV(ni)
      newE(ni) = map(v)
    }
    new Monomial(newV, newE)
  }

  def apply(map: Map[Char, Int]): Monomial = {
    val filtered = map.filter {
      case (k, v) =>
        if (v < 0) throw new Exception("Negative exponents are not allowed")
        v > 0
    }
    fromFilteredMap(filtered)
  }

  def apply(varExps: TraversableOnce[(Char, Int)]): Monomial = {
    import scala.collection.mutable.HashMap
    val map = HashMap.empty[Char, Int]
    for ( (v, e) <- varExps if e != 0 ) {
      if (e < 0) throw new Exception("Negative exponents are not allowed")
      map(v) = map.getOrElse(v, 0) + e
    }
    fromFilteredMap(map)
  }

  object Re {
    val variable = "[\u03B1-\u03C9a-zA-Z]".r
    val varExp = s"($variable)(?:\\^([0-9]+))?".r // we do not allow negative exponents
    val monomial = s"($varExp *)*".r
  }

  def parse(str: String): Monomial = {
    val m: Option[Monomial] = Re.monomial.findFirstMatchIn(str).map { term =>
      val varExps = Re.varExp.findAllMatchIn(term.matched).map {
        _.subgroups match {
          case (List(variable, null)) => (variable(0), 1)
          case (List(variable, exp)) => (variable(0), exp.toInt)
          case _ => throw new Exception("Could not parse term " + term)
        }
      }
      apply(varExps)
    }
    m.getOrElse(throw new Exception("Could not parse term " + str))
  }

/** Default Eq type class for monomial. */
  object equ extends Eq[Monomial] {

    def eqv(x: Monomial, y: Monomial): Boolean = x == y // simple fallback to the equals method

  }

  /** Default `Eq[Monomial]` if no monomial order is selected. */
  implicit def defaultEq(implicit ev: NoImplicit[MonomialOrder]): Eq[Monomial] = equ

  implicit object multiplicativeCMonoid extends MonomialMultiplicativeCMonoid

}

trait MonomialMultiplicativeCMonoid extends MultiplicativeCMonoid[Monomial] {

  def one = Monomial.one

  def times(x: Monomial, y: Monomial): Monomial = x * y

  override def prodn(x: Monomial, n: Int): Monomial = x.pow(n)

  override def isOne(x: Monomial)(implicit ev: Eq[Monomial]): Boolean = x.isOne

}

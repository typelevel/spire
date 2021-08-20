package spire
package math.prime

import spire.implicits._

import spire.math.SafeLong

class PrimeSuite extends munit.FunSuite {
  // val largePrime = SafeLong("393050634124102232869567034555427371542904833")
  // val largeNonPrime = largePrime + 4
  // val tenPrimes = IndexedSeq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29).map(x => SafeLong(x))
  // val nonPrimes = IndexedSeq(10L, 64L, 2L ** 32, 3L ** 10).map(x => SafeLong(x))
  //
  // test("nth") {
  //   for (i <- tenPrimes.indices)
  //     assertEquals(nth(i + 1), tenPrimes(i))
  // }
  //
  // test("isPrime") {
  //   for (p <- tenPrimes)
  //     assert(isPrime(p))
  //   for (n <- nonPrimes)
  //     assert(!isPrime(n))
  // }
  //
  // test("fill") {
  //   assertEquals(fill(10).toSeq, tenPrimes)
  //   assertEquals(fill(2, 2).toSeq, tenPrimes.slice(2, 4))
  // }
  //
  // test("lazyList") {
  //   assertEquals(lazyList.take(10).toList, tenPrimes.toList)
  // }
  //
  // test("factor") {
  //   for (p <- tenPrimes) {
  //     assertEquals(factor(p), Factors(p))
  //     assertEquals(factorPollardRho(p), Factors(p))
  //     assertEquals(factorTrialDivision(p), Factors(p))
  //     assertEquals(factorWheelDivision(p), Factors(p))
  //   }
  //   def terms(f: Factors): Int = f.map(_._2).sum
  //   for (n <- nonPrimes) {
  //     assert(terms(factor(n)) > 1)
  //     assert(terms(factorPollardRho(n)) > 1)
  //     assert(terms(factorTrialDivision(n)) > 1)
  //     assert(terms(factorWheelDivision(n)) > 1)
  //   }
  // }
}

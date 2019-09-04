package spire.math

import java.math.BigInteger

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.NonImplicitAssertions
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import spire.BaseSyntaxTest
import spire.algebra.NRoot
import spire.implicits._
import spire.compat._
import spire.laws.arb._

class BigIntegerNRootTest extends AnyFunSuite with Checkers with BaseSyntaxTest with NonImplicitAssertions {

   private val rootGen: Gen[Int] = Gen.posNum[Int] :| "Root"
   private val bigIntegerGen: Gen[BigInteger] = Arbitrary.arbitrary[BigInteger].map(_.abs()) :| "Base"

   test("NRoot.nroot(n, k) yields the largest number whose k-th power is smaller than or equal to n.")(
      check(Prop.forAllNoShrink(bigIntegerGen, rootGen) { (x: BigInteger, k: Int) =>
         testRootProperApproximation(x, k, NRoot[BigInteger].nroot(x, k))
      }
      )
   )

   def testRootProperApproximation(x: BigInteger, k: Int, rootX: BigInteger): Boolean = {
      rootX.pow(k) <= x && x < (1 + rootX).pow(k)
   }
}
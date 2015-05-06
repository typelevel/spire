package spire.math

import scala.annotation.tailrec

object ContinuedFraction {
	
	def apply(r: Rational): ContinuedFraction = 
		ContinuedFraction( computeFirst(r) )

	// would prefer to use scalaz.NonEmptyList here
	// but: https://groups.google.com/forum/#!topic/typelevel/5wCYLpCpOjc
	private def compute(r: Rational): List[BigInt] = {
		if (r.numerator == 1) List(r.denominator)
		else if (r.denominator == 1) List(r.numerator)
		else if (r.numerator == r.denominator) List(1)
		else {
			if (r.numerator < r.denominator) compute(Rational(r.denominator, r.numerator))
			else if (r.numerator > r.denominator) {
				val remainder = r.numerator % r.denominator
				val divd      = r.numerator / r.denominator
				divd :: compute(Rational(remainder, r.denominator))
			}
			else List(1)
		}
	}

	private def computeFirst(r: Rational): List[BigInt] = 
		if(r.numerator < r.denominator) 0 :: compute(Rational(r.denominator, r.numerator))
		else compute(r)

	// un-safe usage of `head`!
	def first(c: ContinuedFraction): BigInt = 
		c.xs.head

}

case class ContinuedFraction(xs: List[BigInt]) 
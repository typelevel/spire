package spire.laws

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop, Shrink}
import org.scalacheck.Shrink.shrink
import org.scalacheck.util.Pretty

/**
 * Exception thrown when the computation exceeds a type range.
 *
 * For example, when shadowed, Byte(100) + Byte(100) will throw this.
 */
final class InvalidTestException extends Exception

object InvalidTestException {

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, P](f: A1 => P)(implicit p: P => Prop, a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty): Prop =
    Prop.forAllShrink(arbitrary[A1], shrink[A1]) { a1 =>
      try { p(f(a1)) }
      catch {
        case e: InvalidTestException => Prop.passed
      }
    }

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, A2, P](f: (A1, A2) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1],
    s1: Shrink[A1],
    pp1: A1 => Pretty,
    a2: Arbitrary[A2],
    s2: Shrink[A2],
    pp2: A2 => Pretty
  ): Prop =
    forAllSafe((a: A1) => forAllSafe(f(a, _: A2)))

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, A2, A3, P](f: (A1, A2, A3) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1],
    s1: Shrink[A1],
    pp1: A1 => Pretty,
    a2: Arbitrary[A2],
    s2: Shrink[A2],
    pp2: A2 => Pretty,
    a3: Arbitrary[A3],
    s3: Shrink[A3],
    pp3: A3 => Pretty
  ): Prop =
    forAllSafe((a: A1) => forAllSafe(f(a, _: A2, _: A3)))

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, A2, A3, A4, P](f: (A1, A2, A3, A4) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1],
    s1: Shrink[A1],
    pp1: A1 => Pretty,
    a2: Arbitrary[A2],
    s2: Shrink[A2],
    pp2: A2 => Pretty,
    a3: Arbitrary[A3],
    s3: Shrink[A3],
    pp3: A3 => Pretty,
    a4: Arbitrary[A4],
    s4: Shrink[A4],
    pp4: A4 => Pretty
  ): Prop =
    forAllSafe((a: A1) => forAllSafe(f(a, _: A2, _: A3, _: A4)))

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, A2, A3, A4, A5, P](f: (A1, A2, A3, A4, A5) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1],
    s1: Shrink[A1],
    pp1: A1 => Pretty,
    a2: Arbitrary[A2],
    s2: Shrink[A2],
    pp2: A2 => Pretty,
    a3: Arbitrary[A3],
    s3: Shrink[A3],
    pp3: A3 => Pretty,
    a4: Arbitrary[A4],
    s4: Shrink[A4],
    pp4: A4 => Pretty,
    a5: Arbitrary[A5],
    s5: Shrink[A5],
    pp5: A5 => Pretty
  ): Prop =
    forAllSafe((a: A1) => forAllSafe(f(a, _: A2, _: A3, _: A4, _: A5)))

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, A2, A3, A4, A5, A6, P](f: (A1, A2, A3, A4, A5, A6) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1],
    s1: Shrink[A1],
    pp1: A1 => Pretty,
    a2: Arbitrary[A2],
    s2: Shrink[A2],
    pp2: A2 => Pretty,
    a3: Arbitrary[A3],
    s3: Shrink[A3],
    pp3: A3 => Pretty,
    a4: Arbitrary[A4],
    s4: Shrink[A4],
    pp4: A4 => Pretty,
    a5: Arbitrary[A5],
    s5: Shrink[A5],
    pp5: A5 => Pretty,
    a6: Arbitrary[A6],
    s6: Shrink[A6],
    pp6: A6 => Pretty
  ): Prop =
    forAllSafe((a: A1) => forAll(f(a, _: A2, _: A3, _: A4, _: A5, _: A6)))

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, A2, A3, A4, A5, A6, A7, P](f: (A1, A2, A3, A4, A5, A6, A7) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1],
    s1: Shrink[A1],
    pp1: A1 => Pretty,
    a2: Arbitrary[A2],
    s2: Shrink[A2],
    pp2: A2 => Pretty,
    a3: Arbitrary[A3],
    s3: Shrink[A3],
    pp3: A3 => Pretty,
    a4: Arbitrary[A4],
    s4: Shrink[A4],
    pp4: A4 => Pretty,
    a5: Arbitrary[A5],
    s5: Shrink[A5],
    pp5: A5 => Pretty,
    a6: Arbitrary[A6],
    s6: Shrink[A6],
    pp6: A6 => Pretty,
    a7: Arbitrary[A7],
    s7: Shrink[A7],
    pp7: A7 => Pretty
  ): Prop =
    forAllSafe((a: A1) => forAll(f(a, _: A2, _: A3, _: A4, _: A5, _: A6, _: A7)))

  /**
   * Converts a function into a universally quantified property, and catches InvalidTestExceptions
   */
  def forAllSafe[A1, A2, A3, A4, A5, A6, A7, A8, P](f: (A1, A2, A3, A4, A5, A6, A7, A8) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1],
    s1: Shrink[A1],
    pp1: A1 => Pretty,
    a2: Arbitrary[A2],
    s2: Shrink[A2],
    pp2: A2 => Pretty,
    a3: Arbitrary[A3],
    s3: Shrink[A3],
    pp3: A3 => Pretty,
    a4: Arbitrary[A4],
    s4: Shrink[A4],
    pp4: A4 => Pretty,
    a5: Arbitrary[A5],
    s5: Shrink[A5],
    pp5: A5 => Pretty,
    a6: Arbitrary[A6],
    s6: Shrink[A6],
    pp6: A6 => Pretty,
    a7: Arbitrary[A7],
    s7: Shrink[A7],
    pp7: A7 => Pretty,
    a8: Arbitrary[A8],
    s8: Shrink[A8],
    pp8: A8 => Pretty
  ): Prop =
    forAllSafe((a: A1) => forAll(f(a, _: A2, _: A3, _: A4, _: A5, _: A6, _: A7, _: A8)))
}

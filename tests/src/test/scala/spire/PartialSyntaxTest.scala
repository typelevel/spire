package spire

import spire.algebra._
import spire.algebra.partial._
import spire.optional.partialIterable._
import spire.optional.mapIntIntPermutation._
import spire.std.int._
import spire.util._

import org.scalatest.{FunSuite, Matchers, NonImplicitAssertions}
import org.scalatest.prop.Checkers

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

import spire.syntax.eq._
import spire.syntax.cfor._
import spire.std.boolean._

class PartialSyntaxTest extends FunSuite with Checkers with BaseSyntaxTest with NonImplicitAssertions {

  import laws.SpireArbitrary._

  implicit val IntGroup: Group[Int] = implicitly[AdditiveGroup[Int]].additive
  implicit val SeqIntEq: Eq[Seq[Int]] = spire.optional.genericEq.generic[Seq[Int]]

  test("Semigroupoid syntax")(check(forAll { (a: Seq[Int], b: Seq[Int]) => testSemigroupoidSyntax(a, b) }))
  test("Groupoid syntax")(check(forAll { (a: Seq[Int], b: Seq[Int]) => testGroupoidSyntax(a, b) }))
  test("Partial action syntax")(check(forAll { (seq: Seq[Int], perm: Perm) => testPartialActionSyntax(seq, perm.map) }))

  def testSemigroupoidSyntax[A: Semigroupoid: Eq](a: A, b: A) = {
    import spire.syntax.semigroupoid._
    ((a |+|? b) === Semigroupoid[A].partialOp(a, b)) &&
    ((a |+|?? b) === Semigroupoid[A].opIsDefined(a, b))
  }

  def testGroupoidSyntax[A: Groupoid: Eq](a: A, b: A) = {
    import spire.syntax.groupoid._
    (a.isId === Groupoid[A].isId(a)) &&
    (a.leftId === Groupoid[A].leftId(a)) &&
    (a.rightId === Groupoid[A].rightId(a)) &&
    ((a |+|? b) === Groupoid[A].partialOp(a, b)) &&
    ((a |+|?? b) === Groupoid[A].opIsDefined(a, b))
    ((a |-|? b) === Groupoid[A].partialOpInverse(a, b)) &&
    ((a |-|?? b) === Groupoid[A].opInverseIsDefined(a, b))
  }

  def testPartialActionSyntax(seq: Seq[Int], perm: Map[Int, Int]) = {
    import spire.syntax.partialAction._
    ((perm ?|+|> seq) === PartialAction[Seq[Int], Map[Int, Int]].partialActl(perm, seq)) &&
    ((seq <|+|? perm) === PartialAction[Seq[Int], Map[Int, Int]].partialActr(seq, perm)) &&
    ((perm ??|+|> seq) === PartialAction[Seq[Int], Map[Int, Int]].actlIsDefined(perm, seq)) &&
    ((seq <|+|?? perm) === PartialAction[Seq[Int], Map[Int, Int]].actrIsDefined(seq, perm))
  }
}

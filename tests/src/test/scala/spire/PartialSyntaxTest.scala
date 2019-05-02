package spire

import spire.algebra._
import spire.algebra.partial._
import spire.laws.arb._
import spire.optional.Perm
import spire.optional.partialIterable._
import spire.std.boolean._
import spire.std.int._
import spire.syntax.eq._

import org.scalatest.{FunSuite, NonImplicitAssertions}
import org.scalatest.prop.Checkers

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class PartialSyntaxTest extends FunSuite with Checkers with BaseSyntaxTest with NonImplicitAssertions {

  implicit val IntGroup: Group[Int] = implicitly[AdditiveGroup[Int]].additive
  implicit val SeqIntEq: Eq[Seq[Int]] = spire.optional.genericEq.generic[Seq[Int]]

  test("Semigroupoid syntax")(check(forAll { (a: Seq[Int], b: Seq[Int]) => testSemigroupoidSyntax(a, b) }))
  test("Groupoid syntax")(check(forAll { (a: Seq[Int], b: Seq[Int]) => testGroupoidSyntax(a, b) }))
  test("Partial action syntax")(check(forAll { (seq: Seq[Int], perm: Perm) => testPartialActionSyntax(seq, perm) }))

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

  def testPartialActionSyntax(seq: Seq[Int], perm: Perm) = {
    import spire.syntax.partialAction._
    ((perm ?|+|> seq) === PartialAction[Seq[Int], Perm].partialActl(perm, seq)) &&
    ((seq <|+|? perm) === PartialAction[Seq[Int], Perm].partialActr(seq, perm)) &&
    ((perm ??|+|> seq) === PartialAction[Seq[Int], Perm].actlIsDefined(perm, seq)) &&
    ((seq <|+|?? perm) === PartialAction[Seq[Int], Perm].actrIsDefined(seq, perm))
  }
}

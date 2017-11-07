package spire.laws.discipline

import spire.algebra.{Semigroup, Eq}
import spire.laws.SemigroupLaws
import spire.std.option._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait SemigroupTests[A] extends Laws {
  def laws: SemigroupLaws[A]

  def semigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "semigroup",
      None,
      "combine associative" -> forAll(laws.combineAssociative _),
      "combineN1" -> forAll(laws.combineN1 _),
      "combineN2" -> forAll(laws.combineN2 _),
      "combineN3" -> forAll(laws.combineN3 _),
      "tryCombine" -> forAll(laws.tryCombine _)
    )

}

object SemigroupTests {
  def apply[A: Semigroup]: SemigroupTests[A] =
    new SemigroupTests[A] { def laws: SemigroupLaws[A] = SemigroupLaws[A] }
}

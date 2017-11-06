package spire.laws.discipline

import spire.algebra.{Semigroup, Eq}
import spire.laws.SemigroupLaws
import spire.std.option._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait SemigroupTests[A] extends Laws {
  def laws: SemigroupLaws[A]

  def Semigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "Semigroup",
      None,
      "combine associative" -> forAll(laws.combineAssociative _),
      "combine1" -> forAll(laws.combine1 _),
      "combine2" -> forAll(laws.combine2 _),
      "tryCombine" -> forAll(laws.tryCombine _)
    )

}

object SemigroupTests {
  def apply[A: Semigroup]: SemigroupTests[A] =
    new SemigroupTests[A] { def laws: SemigroupLaws[A] = SemigroupLaws[A] }
}

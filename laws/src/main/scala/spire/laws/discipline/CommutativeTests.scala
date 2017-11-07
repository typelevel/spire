package spire.laws.discipline

import org.typelevel.discipline.Laws
import org.scalacheck.Prop

/** A helper trait to define commutative tests that mirrors noncommutative tests. */
trait CommutativeTests { self: Laws =>

  class CommutativeRuleSet(val name: String,
    val noncommutativeParent: RuleSet,
    val commutativeParent: Option[CommutativeRuleSet],
    val props: (String, Prop)*
  ) extends RuleSet {
    val parents = Seq(noncommutativeParent) ++ commutativeParent.toSeq
    val bases = Nil
  }
}

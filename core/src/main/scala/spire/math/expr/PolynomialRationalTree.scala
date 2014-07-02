package spire.math
package expr

import spire.algebra.{EuclideanRing, Field, Trig}
import spire.implicits._

object PolynomialRationalTree extends PolynomialTree with RationalTree {
  object TreeParser extends PolynomialTreeParserTrait with RationalTreeParserTrait {
    lazy val atom: PackratParser[Node] = integer | variable
    def call = failure("Cannot have function calls in polynomial context.")
  }

  object TreeUnparser extends PolynomialTreeUnparserTrait with RationalTreeUnparserTrait 
}

object PolynomialRationalTreeEvaluator extends RingEvaluator[PolynomialRationalTree.type, Polynomial[Rational]] {
  val tree = PolynomialRationalTree
  implicit val scalarAlgebra = Polynomial.ring[Rational]
  override def value(node: tree.Node): Polynomial[Rational] = node match {
    case tree.Variable(power) => (Polynomial.x[Rational])**power
    case tree.RationalNode(r) => Polynomial.constant(r)
    case tree.InvNode(n) =>
      val p = value(n)
      require(p.degree == 0)
      Polynomial.constant(p.maxOrderTermCoeff.reciprocal)
    case _ => super.value(node)
  }
}

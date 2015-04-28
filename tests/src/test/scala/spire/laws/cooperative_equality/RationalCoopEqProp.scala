package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.math.Rational

object RationalCoopEqProp extends Properties("Rational") {
	
	// TODO: make Rational from other types? Only using `Long` here in Utility.scala
	val props: List[Prop] = Utility.props[Rational](Utility.genRational)

	println("Checking Rational's (1) Cooperative Equality, Commutativity for (2) + and (3) *, as well as (4) Transitivity.")
	props.foreach(_.check)

}
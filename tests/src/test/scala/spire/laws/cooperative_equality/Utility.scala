package spire.laws.cooperative_equality

import org.scalacheck.{Prop, Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

import spire.algebra.Rig
import spire.implicits._

object Utility {

  def coopEquals[A](x: A, y: A) = x == y && x.hashCode == y.hashCode

  def props[A : Rig](g: Gen[A]): List[Prop] = {
    val commutativeAdd = forAll(g, g) { (a: A, b: A) =>
      coopEquals(a+b,b+a) 
    }
    val commutativeMult = forAll(g, g) { (a: A, b: A) =>
      coopEquals(a*b,b*a) 
    }
    val transitive = forAll(g, g, g) { (a: A, b: A, c: A) =>
      if( coopEquals(a, b) && coopEquals(b, c)) coopEquals(a, c) else true
    }
    List(commutativeAdd, commutativeMult, transitive)
  }
}
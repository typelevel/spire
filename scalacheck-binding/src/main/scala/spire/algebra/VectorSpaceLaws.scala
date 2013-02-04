package spire.algebra

import spire.implicits._
import spire.math.{ Eq, Order }

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop._

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VectorSpaceLaws[V, A] {

  implicit def scalar(implicit V: Module[V, A]): Ring[A] = V.scalar

  val vectorLaws: Laws[V]
  val scalarLaws: Laws[A]

  import vectorLaws.{ Eq => EqV, Arbitrary => ArbV }
  import scalarLaws.{ Eq => EqA, Arbitrary => ArbA }

  def module(implicit V: Module[V, A]) = new Properties("module") {
    include(vectorLaws.abGroup(V.additive))
    include(scalarLaws.ring(V.scalar))

    property("associative scalar") = forAll { (r: A, s: A, v: V) =>
      val w = r *: s *: v
      w === ((r * s) *: v)
    }
    property("scalar is distributes over vector") = forAll { (r: A, v: V, w: V) =>
      (r *: (v + w)) === ((r *: v) + (r *: w))
    }
    property("vector distributes over scalar") = forAll { (r: A, s: A, v: V) =>
      ((r + s) *: v) === ((r *: v) + (s *: v))
    }
    property("scalar identity is identity") = forAll { (v: V) =>
      (V.scalar.one *: v) === v
    }
  }

  def vectorSpace(implicit V: VectorSpace[V, A]) = new Properties("vector space") {
    include(module)
    include(scalarLaws.field(V.scalar))
  }

  def normedVectorSpace(implicit V: NormedVectorSpace[V, A], ev0: Order[A], ev1: Signed[A]) = new Properties("normed vector space") {
    include(vectorSpace)
    property("scalable") = forAll { (a: A, v: V) =>
      a.abs * v.norm === (a.abs *: v).norm
    }
    property("triangle inequality") = forAll { (v: V, w: V) =>
      (v + w).norm <= (v.norm + w.norm)
    }
    property("only 1 zero") = forAll { (v: V) =>
      if (v === V.zero) {
        v.norm === Ring[A].zero
      } else {
        v.norm > Ring[A].zero
      }
    }
  }

  def linearity(f: V => A)(implicit V: Module[V, A]) = new Properties("linearity") {
    property("homogeneity") = forAll((r: A, v: V) => f(r *: v) === r * f(v))
    property("additivity") = forAll((v: V, w: V) => f(v + w) === f(v) + f(w))
  }

  def innerProductSpace(implicit V: InnerProductSpace[V, A], A: Order[A], A0: Signed[A]) = new Properties("inner-product space") {
    include(vectorSpace)

    property("symmetry") = forAll { (v: V, w: V) =>
      (v ⋅ w).abs === (w ⋅ v).abs
    }
    property("linearity of partial inner product") = forAll { (w: V) =>
      linearity(_ ⋅ w)
    }
  }
}

object VectorSpaceLaws {
  def apply[V: Eq: Arbitrary, A: Eq: Arbitrary] = new VectorSpaceLaws[V, A] {
    val vectorLaws = Laws[V]
    val scalarLaws = Laws[A]
  }
}

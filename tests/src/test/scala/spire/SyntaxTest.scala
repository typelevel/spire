package spire

import spire.math.Rational
import spire.algebra._
import spire.std.int._
import spire.std.double._
import spire.std.seq._
import spire.std.string._

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class SyntaxTest extends FunSuite with Checkers with BaseSyntaxTest {
  // This tests 2 things:
  //  1) That the ops work as they're suppose to,
  //  2) That this actually compiles (ie. ops exist at all, given an import).

  case class NonZero[A](val x: A)

  implicit def ArbNonZero[A: Ring: Eq: Arbitrary]: Arbitrary[NonZero[A]] = {
    import spire.syntax.eq._
    Arbitrary(arbitrary[A].filter(_ =!= Ring[A].zero).map(NonZero[A](_)))
  }

  case class Positive[A](val x: A)

  implicit def ArbPositive[A: Ring: Signed: Arbitrary]: Arbitrary[Positive[A]] = {
    import spire.syntax.signed._
    Arbitrary(arbitrary[A].map(_.abs).filter(_.sign != Sign.Zero).map(Positive(_)))
  }

  implicit def ArbVector[A: Arbitrary]: Arbitrary[Vector[A]] = Arbitrary(for {
    x <- arbitrary[A]
    y <- arbitrary[A]
    z <- arbitrary[A]
  } yield Vector(x, y, z))

  import spire.laws.arb.rational

  test("Eq syntax")(check(forAll { (a: Int, b: Int) => testEqSyntax(a, b) }))
  test("Partial order syntax")(check(forAll { (a: Int, b: Int) => testPartialOrderSyntax(a, b) }))
  test("Order syntax")(check(forAll { (a: Int, b: Int) => testOrderSyntax(a, b) }))
  test("Signed syntax")(check(forAll { (a: Int) => testSignedSyntax(a) }))
  test("IsReal syntax")(check(forAll { (a: Double) => testIsRealSyntax(a) }))
  test("Semigroup syntax")(check(forAll { (a: String, b: String) => testSemigroupSyntax(a, b) }))
  test("Monoid syntax")(check(forAll { (a: String, b: String) => testMonoidSyntax(a, b) }))
  test("Group syntax")(check(forAll { (a: Int, b: Int) => testMonoidSyntax(a, b)(Group.additive[Int]) }))
  test("AdditiveSemigroup syntax")(check(forAll { (a: Int, b: Int) => testAdditiveSemigroupSyntax(a, b) }))
  test("AdditiveMonoid syntax")(check(forAll { (a: Int, b: Int) => testAdditiveMonoidSyntax(a, b) }))
  test("AdditiveGroup syntax")(check(forAll { (a: Int, b: Int) => testAdditiveGroupSyntax(a, b) }))
  test("MultiplicativeSemigroup syntax")(check(forAll { (a: Int, b: Int) => testMultiplicativeSemigroupSyntax(a, b) }))
  test("MultiplicativeMonoid syntax")(check(forAll { (a: Int, b: Int) => testMultiplicativeMonoidSyntax(a, b) }))
  test("MultiplicativeGroup syntax")(check(forAll { (a: Double, b: NonZero[Double]) =>
    testMultiplicativeGroupSyntax(a, b.x)
  }))
  test("Semiring syntax")(check(forAll { (a: Int, b: Int) => testSemiringSyntax(a, b) }))
  test("Rig syntax")(check(forAll { (a: Int, b: Int) => testRigSyntax(a, b) }))
  test("Rng syntax")(check(forAll { (a: Int, b: Int) => testRngSyntax(a, b) }))
  test("Ring syntax")(check(forAll { (a: Int, b: Int) => testRingSyntax(a, b) }))
  test("EuclideanRing syntax")(check(forAll { (a: Int, b: NonZero[Int]) => testEuclideanRingSyntax(a, b.x) }))
  test("Field syntax")(check(forAll { (a: Double, b: NonZero[Double]) =>
    testFieldSyntax(a, b.x)(implicitly, spire.optional.totalfloat.TotalDoubleOrder)
  }))
  test("NRoot syntax")(check(forAll { (a: Positive[Double]) => testNRootSyntax(a.x) }))
  test("Module syntax")(check(forAll { (v: Vector[Int], w: Vector[Int], a: Int) => testModuleSyntax(v, w, a) }))
  test("VectorSpace syntax")(check(forAll { (v: Vector[Double], w: Vector[Double], a: NonZero[Double]) =>
    testVectorSpaceSyntax(v, w, a.x)
  }))
  test("NormedVectorSpace syntax")(check(forAll { (v: Vector[Double], w: Vector[Double], a: NonZero[Double]) =>
    testNormedVectorSpaceSyntax(v, w, a.x)
  }))
  test("InnerProductSpace syntax")(check(forAll { (v: Vector[Rational], w: Vector[Rational], a: NonZero[Rational]) =>
    testInnerProductSpaceSyntax(v, w, a.x)
  }))
  test("CoordinateSpace syntax")(check(forAll { (v: Vector[Rational], w: Vector[Rational], a: NonZero[Rational]) =>
    testCoordinateSpaceSyntax(v, w, a.x)(CoordinateSpace.seq[Rational, Vector](3))
  }))
  test("Bool syntax")(check(forAll { (a: Int, b: Int) => testBoolSyntax(a, b) }))
}

trait BaseSyntaxTest {
  def testEqSyntax[A: Eq](a: A, b: A) = {
    import spire.syntax.eq._
    ((a === b) == Eq[A].eqv(a, b)) &&
      ((a =!= b) == Eq[A].neqv(a, b))
  }

  def testPartialOrderSyntax[A: PartialOrder](a: A, b: A) = {
    import spire.syntax.order._
    ((a === b) == PartialOrder[A].eqv(a, b)) &&
      ((a =!= b) == PartialOrder[A].neqv(a, b)) &&
      ((a < b) == PartialOrder[A].lt(a, b)) &&
      ((a > b) == PartialOrder[A].gt(a, b)) &&
      ((a <= b) == PartialOrder[A].lteqv(a, b)) &&
      ((a >= b) == PartialOrder[A].gteqv(a, b)) &&
      ((a pmin b) == PartialOrder[A].pmin(a, b)) &&
      ((a pmax b) == PartialOrder[A].pmax(a, b)) &&
      ((a partialCompare b) == PartialOrder[A].partialCompare(a, b)) &&
      ((a tryCompare b) == PartialOrder[A].tryCompare(a, b))
  }

  def testOrderSyntax[A: Order](a: A, b: A) = {
    import spire.syntax.order._
    ((a === b) == Order[A].eqv(a, b)) &&
      ((a =!= b) == Order[A].neqv(a, b)) &&
      ((a < b) == Order[A].lt(a, b)) &&
      ((a > b) == Order[A].gt(a, b)) &&
      ((a <= b) == Order[A].lteqv(a, b)) &&
      ((a >= b) == Order[A].gteqv(a, b)) &&
      ((a min b) == Order[A].min(a, b)) &&
      ((a max b) == Order[A].max(a, b)) &&
      ((a compare b) == Order[A].compare(a, b))
  }

  def testSignedSyntax[A: Signed](a: A) = {
    import spire.syntax.signed._
    (a.sign == Signed[A].sign(a)) &&
      (a.signum == Signed[A].signum(a)) &&
      (a.abs == Signed[A].abs(a)) &&
      (a.isSignZero == Signed[A].isSignZero(a)) &&
      (a.isSignPositive == Signed[A].isSignPositive(a)) &&
      (a.isSignNegative == Signed[A].isSignNegative(a)) &&
      (a.isSignNonZero == Signed[A].isSignNonZero(a)) &&
      (a.isSignNonPositive == Signed[A].isSignNonPositive(a)) &&
      (a.isSignNonNegative == Signed[A].isSignNonNegative(a))
  }

  def testIsRealSyntax[A: IsReal](a: A) = {
    import spire.syntax.isReal._
    (a.ceil == IsReal[A].ceil(a)) &&
      (a.floor == IsReal[A].floor(a)) &&
      (a.round == IsReal[A].round(a)) &&
      (a.isWhole == IsReal[A].isWhole(a))
  }

  def testSemigroupSyntax[A: Semigroup](a: A, b: A) = {
    import spire.syntax.semigroup._
    ((a |+| b) == Semigroup[A].op(a, b))
  }

  def testMonoidSyntax[A: Monoid](a: A, b: A) = {
    import spire.syntax.monoid._
    ((a |+| b) == Monoid[A].op(a, b))
  }

  def testGroupSyntax[A: Group](a: A, b: A) = {
    import spire.syntax.group._
    ((a |+| b) == Group[A].op(a, b)) &&
      ((a |-| b) == Group[A].opInverse(a, b)) &&
      (a.inverse == Group[A].inverse(a))
  }

  def testAdditiveSemigroupSyntax[A: AdditiveSemigroup](a: A, b: A) = {
    import spire.syntax.additiveSemigroup._
    ((a + b) == implicitly[AdditiveSemigroup[A]].plus(a, b))
  }

  def testAdditiveMonoidSyntax[A: AdditiveMonoid](a: A, b: A) = {
    import spire.syntax.additiveMonoid._
    ((a + b) == implicitly[AdditiveMonoid[A]].plus(a, b))
  }

  def testAdditiveGroupSyntax[A: AdditiveGroup](a: A, b: A) = {
    import spire.syntax.additiveGroup._
    ((a + b) == implicitly[AdditiveGroup[A]].plus(a, b)) &&
      ((a - b) == implicitly[AdditiveGroup[A]].minus(a, b)) &&
      (-a == implicitly[AdditiveGroup[A]].negate(a))
  }

  def testMultiplicativeSemigroupSyntax[A: MultiplicativeSemigroup](a: A, b: A) = {
    import spire.syntax.multiplicativeSemigroup._
    ((a * b) == implicitly[MultiplicativeSemigroup[A]].times(a, b))
  }

  def testMultiplicativeMonoidSyntax[A: MultiplicativeMonoid](a: A, b: A) = {
    import spire.syntax.multiplicativeMonoid._
    ((a * b) == implicitly[MultiplicativeMonoid[A]].times(a, b))
  }

  def testMultiplicativeGroupSyntax[A: MultiplicativeGroup](a: A, b: A) = {
    import spire.syntax.multiplicativeGroup._
    ((a * b) == implicitly[MultiplicativeGroup[A]].times(a, b)) &&
      ((a / b) == implicitly[MultiplicativeGroup[A]].div(a, b)) &&
      (a.reciprocal == implicitly[MultiplicativeGroup[A]].reciprocal(a))
  }

  def testSemiringSyntax[A: Semiring](a: A, b: A) = {
    import spire.syntax.semiring._
    ((a + b) == Semiring[A].plus(a, b)) &&
      ((a * b) == Semiring[A].times(a, b)) &&
      ((a ** 2) == Semiring[A].pow(a, 2)) &&
      ((a pow 2) == Semiring[A].pow(a, 2))
  }

  def testRigSyntax[A: Rig](a: A, b: A) = {
    import spire.syntax.rig._
    ((a + b) == Rig[A].plus(a, b)) &&
      ((a * b) == Rig[A].times(a, b)) &&
      ((a ** 2) == Rig[A].pow(a, 2)) &&
      ((a pow 2) == Rig[A].pow(a, 2))
  }

  def testRngSyntax[A: Rng](a: A, b: A) = {
    import spire.syntax.rng._
    ((a + b) == Rng[A].plus(a, b)) &&
      ((a - b) == Rng[A].minus(a, b)) &&
      (-a == Rng[A].negate(a)) &&
      ((a * b) == Rng[A].times(a, b)) &&
      ((a ** 2) == Rng[A].pow(a, 2)) &&
      ((a pow 2) == Rng[A].pow(a, 2))
  }

  def testRingSyntax[A: Ring](a: A, b: A) = {
    import spire.syntax.ring._
    ((a + b) == Ring[A].plus(a, b)) &&
      ((a - b) == Ring[A].minus(a, b)) &&
      (-a == Ring[A].negate(a)) &&
      ((a * b) == Ring[A].times(a, b)) &&
      ((a ** 2) == Ring[A].pow(a, 2)) &&
      ((a pow 2) == Ring[A].pow(a, 2)) &&
      ((a + 42) == Ring[A].plus(a, Ring[A].fromInt(42))) &&
      ((42 + a) == Ring[A].plus(Ring[A].fromInt(42), a)) &&
      ((a - 42) == Ring[A].minus(a, Ring[A].fromInt(42))) &&
      ((42 - a) == Ring[A].minus(Ring[A].fromInt(42), a)) &&
      ((a * 42) == Ring[A].times(a, Ring[A].fromInt(42))) &&
      ((42 * a) == Ring[A].times(Ring[A].fromInt(42), a))
  }

  def testEuclideanRingSyntax[A: EuclideanRing](a: A, b: A) = {
    import spire.syntax.euclideanRing._
    ((a + b) == Ring[A].plus(a, b)) &&
      ((a - b) == Ring[A].minus(a, b)) &&
      (-a == Ring[A].negate(a)) &&
      ((a * b) == Ring[A].times(a, b)) &&
      ((a /~ b) == EuclideanRing[A].quot(a, b)) &&
      ((a % b) == EuclideanRing[A].mod(a, b)) &&
      ((a /% b) == EuclideanRing[A].quotmod(a, b)) &&
      ((a ** 2) == Ring[A].pow(a, 2)) &&
      ((a pow 2) == Ring[A].pow(a, 2)) &&
      ((a gcd b) == EuclideanRing[A].gcd(a, b)) &&
      ((a lcm b) == EuclideanRing[A].lcm(a, b)) &&
      ((a + 42) == Ring[A].plus(a, Ring[A].fromInt(42))) &&
      ((42 + a) == Ring[A].plus(Ring[A].fromInt(42), a)) &&
      ((a - 42) == Ring[A].minus(a, Ring[A].fromInt(42))) &&
      ((42 - a) == Ring[A].minus(Ring[A].fromInt(42), a)) &&
      ((a * 42) == Ring[A].times(a, Ring[A].fromInt(42))) &&
      ((42 * a) == Ring[A].times(Ring[A].fromInt(42), a)) &&
      ((a /~ 42) == EuclideanRing[A].quot(a, Ring[A].fromInt(42))) &&
      ((42 /~ b) == EuclideanRing[A].quot(Ring[A].fromInt(42), b)) &&
      ((a % 42) == EuclideanRing[A].mod(a, Ring[A].fromInt(42))) &&
      ((42 % b) == EuclideanRing[A].mod(Ring[A].fromInt(42), b))
  }

  def testFieldSyntax[A: Field: Eq](a: A, b: A) = {
    import spire.syntax.eq._
    import spire.syntax.field._
    import spire.std.tuples._

    ((42 + a) === Ring[A].plus(Ring[A].fromInt(42), a)) &&
    ((42 - a) === Ring[A].minus(Ring[A].fromInt(42), a)) &&
    ((3.14 + b) === Ring[A].plus(Field[A].fromDouble(3.14), b)) &&
    ((3.14 - b) === Ring[A].minus(Field[A].fromDouble(3.14), b)) &&
    ((a + b) === Ring[A].plus(a, b)) &&
    ((a - b) === Ring[A].minus(a, b)) &&
    (-a === Ring[A].negate(a)) &&
    ((a * b) === Ring[A].times(a, b)) &&
    ((a /~ b) === EuclideanRing[A].quot(a, b)) &&
    ((a % b) === EuclideanRing[A].mod(a, b)) &&
    ((a /% b) === EuclideanRing[A].quotmod(a, b)) &&
    ((a / b) === Field[A].div(a, b)) &&
    ((a ** 2) === Ring[A].pow(a, 2)) &&
    ((a pow 2) === Ring[A].pow(a, 2)) &&
    ((a gcd b) === EuclideanRing[A].gcd(a, b)) &&
    ((a lcm b) === EuclideanRing[A].lcm(a, b)) &&
    ((a + 42) === Ring[A].plus(a, Ring[A].fromInt(42))) &&
    ((a - 42) === Ring[A].minus(a, Ring[A].fromInt(42))) &&
    ((a * 42) === Ring[A].times(a, Ring[A].fromInt(42))) &&
    ((42 * a) === Ring[A].times(Ring[A].fromInt(42), a)) &&
    ((a /~ 42) === EuclideanRing[A].quot(a, Ring[A].fromInt(42))) &&
    ((42 /~ b) === EuclideanRing[A].quot(Ring[A].fromInt(42), b)) &&
    ((a % 42) === EuclideanRing[A].mod(a, Ring[A].fromInt(42))) &&
    ((42 % b) === EuclideanRing[A].mod(Ring[A].fromInt(42), b)) &&
    ((a + 3.14) === Ring[A].plus(a, Field[A].fromDouble(3.14))) &&
    ((a - 3.14) === Ring[A].minus(a, Field[A].fromDouble(3.14))) &&
    ((a * 3.14) === Ring[A].times(a, Field[A].fromDouble(3.14))) &&
    ((3.14 * b) === Ring[A].times(Field[A].fromDouble(3.14), b)) &&
    ((a / 3.14) === Field[A].div(a, Field[A].fromDouble(3.14))) &&
    ((3.14 / b) === Field[A].div(Field[A].fromDouble(3.14), b))
  }

  def testNRootSyntax[A: NRoot: Field](a: A) = {
    import spire.syntax.nroot._
    val half = Field[A].fromDouble(0.5)
    (a.sqrt == NRoot[A].sqrt(a)) &&
      ((a nroot 5) == NRoot[A].nroot(a, 5)) &&
      ((a fpow half) == NRoot[A].fpow(a, half)) &&
      ((a ** 0.5) == NRoot[A].fpow(a, half))
  }

  def testModuleSyntax[V, A](v: V, w: V, a: A)(implicit V: Module[V, A], A: Ring[A]) = {
    import spire.syntax.module._
    ((v + w) == V.plus(v, w)) &&
      ((v - w) == V.minus(v, w)) &&
      (-v == V.negate(v)) &&
      ((a *: v) == V.timesl(a, v)) &&
      ((v :* a) == V.timesr(v, a)) &&
      //((2 *: v) == V.timesl(A.fromInt(2), v)) &&
      ((v :* 2) == V.timesr(v, A.fromInt(2)))
  }

  def testVectorSpaceSyntax[V, A](v: V, w: V, a: A)(implicit V: VectorSpace[V, A]) = {
    import spire.syntax.vectorSpace._
    implicit def A: Field[A] = V.scalar
    ((v + w) == V.plus(v, w)) &&
      ((v - w) == V.minus(v, w)) &&
      (-v == V.negate(v)) &&
      ((a *: v) == V.timesl(a, v)) &&
      ((v :* a) == V.timesr(v, a))
      //((2 *: v) == V.timesl(A.fromInt(2), v)) &&
      //((v :* 2) == V.timesr(v, A.fromInt(2))) &&
      //((0.5 *: v) == V.timesl(A.fromDouble(0.5), v)) &&
      //((v :* 0.5) == V.timesr(v, A.fromDouble(0.5))) &&
      //((v :/ 2) == V.divr(v, A.fromInt(2)))
  }

  def testNormedVectorSpaceSyntax[V, A](v: V, w: V, a: A)(implicit V: NormedVectorSpace[V, A]) = {
    import spire.syntax.normedVectorSpace._
    implicit def A: Field[A] = V.scalar
    ((v + w) == V.plus(v, w)) &&
      ((v - w) == V.minus(v, w)) &&
      (-v == V.negate(v)) &&
      ((a *: v) == V.timesl(a, v)) &&
      ((v :* a) == V.timesr(v, a)) &&
      //((2 *: v) == V.timesl(A.fromInt(2), v)) &&
      //((v :* 2) == V.timesr(v, A.fromInt(2))) &&
      //((0.5 *: v) == V.timesl(A.fromDouble(0.5), v)) &&
      //((v :* 0.5) == V.timesr(v, A.fromDouble(0.5))) &&
      //((v :/ 2) == V.divr(v, A.fromInt(2))) &&
      (v.norm == V.norm(v)) &&
      ((V.norm(v) == A.zero) || (v.normalize == V.normalize(v)))
  }

  def testInnerProductSpaceSyntax[V, A](v: V, w: V, a: A)(implicit V: InnerProductSpace[V, A]) = {
    import spire.syntax.innerProductSpace._
    implicit def A: Field[A] = V.scalar
    ((v + w) == V.plus(v, w)) &&
      ((v - w) == V.minus(v, w)) &&
      (-v == V.negate(v)) &&
      ((a *: v) == V.timesl(a, v)) &&
      ((v :* a) == V.timesr(v, a)) &&
      //((2 *: v) == V.timesl(A.fromInt(2), v)) &&
      //((v :* 2) == V.timesr(v, A.fromInt(2))) &&
      //((0.5 *: v) == V.timesl(A.fromDouble(0.5), v)) &&
      //((v :* 0.5) == V.timesr(v, A.fromDouble(0.5))) &&
      //((v :/ 2) == V.divr(v, A.fromInt(2))) &&
      ((v dot w) == V.dot(v, w)) &&
      ((v ⋅ w) == V.dot(v, w))
  }

  def testCoordinateSpaceSyntax[V, A](v: V, w: V, a: A)(implicit V: CoordinateSpace[V, A]) = {
    import spire.syntax.coordinateSpace._
    implicit def A: Field[A] = V.scalar
    ((v + w) == V.plus(v, w)) &&
      ((v - w) == V.minus(v, w)) &&
      (-v == V.negate(v)) &&
      ((a *: v) == V.timesl(a, v)) &&
      ((v :* a) == V.timesr(v, a)) &&
      //((2 *: v) == V.timesl(A.fromInt(2), v)) &&
      //((v :* 2) == V.timesr(v, A.fromInt(2))) &&
      //((0.5 *: v) == V.timesl(A.fromDouble(0.5), v)) &&
      //((v :* 0.5) == V.timesr(v, A.fromDouble(0.5))) &&
      //((v :/ 2) == V.divr(v, A.fromInt(2))) &&
      ((v dot w) == V.dot(v, w)) &&
      ((v ⋅ w) == V.dot(v, w)) &&
      (v._x == V._x(v)) &&
      (v._y == V._y(v)) &&
      (v._z == V._z(v)) &&
      (v.coord(0) == V.coord(v, 0)) &&
      (v.coord(1) == V.coord(v, 1))
  }

  def testBoolSyntax[A: Bool](a: A, b: A) = {
    import spire.syntax.bool._
    ((a & b) == Bool[A].and(a, b)) &&
      ((a | b) == Bool[A].or(a, b)) &&
      ((a ^ b) == Bool[A].xor(a, b)) &&
      (~a == Bool[A].complement(a))
  }
}

class PartialOrderSyntaxTest extends FunSuite with Matchers with Checkers {
  import spire.algebra.PartialOrder
  import spire.syntax.all._
  object intDivisibility extends PartialOrder[Int] {
    def partialCompare(a: Int, b: Int) = {
      if (a == b)
        0.0
      else if (b % a == 0)
        -1.0
      else if (a % b == 0)
        1.0
      else
        Double.NaN
    }
  }

  test("With intDivisibility: Seq(2,3,6,9,12).pmin sameElements Seq(2,3)") {
    Seq(2,3,6,9,12).pmin(intDivisibility).sameElements(Seq(2,3)) shouldBe true
  }
  test("With intDivisibility: Seq(2,3,6,9,12).pmax sameElements Seq(9,12)") {
    Seq(2,3,6,9,12).pmax(intDivisibility).sameElements(Seq(9,12)) shouldBe true
  }

  case class PosInt(val x: Int)

  implicit def ArbPosInt: Arbitrary[PosInt] = Arbitrary(Gen.choose(1, 30).map(PosInt))

  test("pmin")(check(forAll { (posSeq: Seq[PosInt]) =>
    val seq = posSeq.map(_.x)
    def isMinimal(i: Int) = seq.forall(j => !(intDivisibility.partialCompare(i, j) > 0))
    seq.pmin(intDivisibility).toSet === seq.filter(isMinimal).toSet
  }))

  test("pmax")(check(forAll { (posSeq: Seq[PosInt]) =>
    val seq = posSeq.map(_.x)
    def isMaximal(i: Int) = seq.forall(j => !(intDivisibility.partialCompare(i, j) < 0))
    seq.pmax(intDivisibility).toSet === seq.filter(isMaximal).toSet
  }))
}

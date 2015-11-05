package spire
package test.scala.spire.math

import spire.algebra._
import spire.math._
import spire.implicits._

import java.math.MathContext

import org.scalatest.FunSuite


/**
 * Just some sanity tests to make sure that the type classes we expect to exist
 * actually do exist.
 */
class TypeclassExistenceTest extends FunSuite {

  def hasAbGroup[A](implicit g: AbGroup[A] = null, m: ClassTag[A]): Unit = {
    assert(g != null, "Expected implicit AbGroup[%s] instance, but it was not found." format m)
  }

  def hasRig[A](implicit rig: Rig[A] = null, m: ClassTag[A]): Unit = {
    assert(rig != null, "Expected implicit Rig[%s] instance, but it was not found." format m)
  }

  def hasRing[A](implicit ring: Ring[A] = null, m: ClassTag[A]): Unit = {
    assert(ring != null, "Expected implicit Ring[%s] instance, but it was not found." format m)
  }

  def hasEuclideanRing[A](implicit e: EuclideanRing[A] = null, m: ClassTag[A]): Unit = {
    assert(e!= null, "Expected implicit EuclideanRing[%s] instance, but it was not found." format m)
  }

  def hasField[A](implicit f: Field[A] = null, m: ClassTag[A]): Unit = {
    assert(f != null, "Expected implicit Field[%s] instance, but it was not found." format m)
  }

  def hasNumeric[A](implicit n: Numeric[A] = null, m: ClassTag[A]): Unit = {
    assert(n != null, "Expected implicit Numeric[%s] instance, but it was not found." format m)
  }

  def hasFractional[A](implicit f: Fractional[A] = null, m: ClassTag[A]): Unit = {
    assert(f != null, "Expected implicit Fractional[%s] instance, but it was not found." format m)
  }

  def hasOrder[A](implicit ev: Order[A] = null, m: ClassTag[A]): Unit = {
    assert(ev != null, "Expected implicit Order[%s] instance, but it was not found." format m)
  }

  def hasEq[A](implicit ev: Eq[A] = null, m: ClassTag[A]): Unit = {
    assert(ev != null, "Expected implicit Eq[%s] instance, but it was not found." format m)
  }

  def hasConvertableFrom[A](implicit ev: ConvertableFrom[A] = null, m: ClassTag[A]): Unit = {
    assert(ev != null, "Expected implicit ConvertableFrom[%s] instance, but it was not found." format m)
  }

  def hasConvertableTo[A](implicit ev: ConvertableTo[A] = null, m: ClassTag[A]): Unit = {
    assert(ev != null, "Expected implicit ConvertableTo[%s] instance, but it was not found." format m)
  }

  def hasNRoot[A](implicit ev: NRoot[A] = null, m: ClassTag[A]): Unit = {
    assert(ev != null, "Expected implicit NRoot[%s] instance, but it was not found." format m)
  }

  test("Numeric is ConvertableTo") {
    def check[A: Numeric : ClassTag]: Unit = {
      hasConvertableTo[A]
    }

    check[Int]
  }

  test("Numeric is ConvertableFrom") {
    def check[A: Numeric : ClassTag]: Unit = {
      hasConvertableFrom[A]
    }

    check[Int]
  }

  test("Rings are Rigs") {
    def check[A: Ring: ClassTag]: Unit = {
      hasRig[A]
    }

    check[Int]
  }

  test("EuclideanRings are Rings") {
    def check[A: EuclideanRing: ClassTag]: Unit = {
      hasRing[A]
    }

    check[Int]
  }

  test("Fields are EuclideanRings") {
    def check[A: Field: ClassTag]: Unit = {
      hasEuclideanRing[A]
    }

    check[Double]
  }

  test("Numerics have Order, NRoot, and are Rigs") {
    def check[A: Numeric: ClassTag]: Unit = {
      hasRig[A]
      hasOrder[A]
      hasNRoot[A]
    }

    check[Int]
  }

  test("Fractional have Order, NRoot and are Fields") {
    def check[A: Fractional: ClassTag]: Unit = {
      hasOrder[A]
      hasEuclideanRing[A]
      hasField[A]
      hasNRoot[A]
    }

    check[Double]
  }

  test("Unit has Eq:Order") {
    hasEq[Unit]
    hasOrder[Unit]
    hasAbGroup[Unit]
  }

  test("UByte has Eq:Order:Rig") {
    hasEq[UByte]
    hasOrder[UByte]
    hasRig[UByte]
  }

  test("UShort has Eq:Order:Rig") {
    hasEq[UShort]
    hasOrder[UShort]
    hasRig[UShort]
  }

  test("UInt has Eq:Order:Rig") {
    hasEq[UInt]
    hasOrder[UInt]
    hasRig[UInt]
  }

  test("ULong has Eq:Order:Rig") {
    hasEq[ULong]
    hasOrder[ULong]
    hasRig[ULong]
  }

  test("Int has Eq:Order:EuclideanRing:NRoot") {
    hasEq[Int]
    hasOrder[Int]
    hasRing[Int]
    hasEuclideanRing[Int]
    hasNRoot[Int]
  }

  test("Long has Eq:Order:EuclideanRing:NRoot") {
    hasEq[Long]
    hasOrder[Long]
    hasRing[Long]
    hasEuclideanRing[Long]
    hasNRoot[Long]
  }

  test("BigInt has Eq:EuclideanRing:NRoot") {
    hasEq[BigInt]
    hasOrder[BigInt]
    hasRing[BigInt]
    hasEuclideanRing[BigInt]
    hasNRoot[BigInt]
  }

  test("Float is FieldWithNRoot") {
    hasEq[Float]
    hasRing[Float]
    hasEuclideanRing[Float]
    hasField[Float]
    hasNRoot[Float]
  }

  test("Double is FieldWithNRoot") {
    hasEq[Double]
    hasRing[Double]
    hasEuclideanRing[Double]
    hasField[Double]
    hasNRoot[Double]
  }

  test("BigDecimal is FieldWithNRoot") {
    hasEq[BigDecimal]
    hasRing[BigDecimal]
    hasEuclideanRing[BigDecimal]
    hasField[BigDecimal]
    hasNRoot[BigDecimal]
  }

  test("Rational is Field") {
    hasEq[Rational]
    hasRing[Rational]
    hasEuclideanRing[Rational]
    hasField[Rational]
  }

  test("Algebraic is FieldWithNRoot") {
    hasEq[Algebraic]
    hasRing[Algebraic]
    hasEuclideanRing[Algebraic]
    hasField[Algebraic]
    hasNRoot[Algebraic]
  }

  test("Everybody is Numeric") {
    hasNumeric[Int]
    hasNumeric[Long]
    hasNumeric[BigInt]
    hasNumeric[Float]
    hasNumeric[Double]
    hasNumeric[BigDecimal]
    hasNumeric[Rational]
    hasNumeric[Algebraic]
  }

  test("Float, Double, Rational, BigDecimal, and Algebraic are Fractional") {
    hasFractional[Float]
    hasFractional[Double]
    hasFractional[BigDecimal]
    hasFractional[Rational]
    hasFractional[Algebraic]
  }

  test("NRoot[Rational] requires implicit ApproximationContext") {
    def check[A](implicit e: NRoot[A] = null): Unit = {
      assert(e == null)
    }

    check[Rational]
  }
}



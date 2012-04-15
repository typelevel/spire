package test.scala.spire.math

import spire.algebra._
import spire.math._
import Implicits._

import java.math.MathContext

import org.scalatest.FunSuite


/**
 * Just some sanity tests to make sure that the type classes we expect to exist
 * actually do exist.
 */
class TypeclassExistenceTest extends FunSuite {

  def hasRing[A](implicit ring: Ring[A] = null, m: Manifest[A]) {
    assert(ring != null, "Expected implicit Ring[%s] instance, but it was not found." format m)
  }
   
  def hasEuclideanRing[A](implicit e: EuclideanRing[A] = null, m: Manifest[A]) {
    assert(e!= null, "Expected implicit EuclideanRing[%s] instance, but it was not found." format m)
  }

  def hasField[A](implicit f: Field[A] = null, m: Manifest[A]) {
    assert(f != null, "Expected implicit Field[%s] instance, but it was not found." format m)
  }

  def hasNumeric[A](implicit n: Numeric[A] = null, m: Manifest[A]) {
    assert(n != null, "Expected implicit Numeric[%s] instance, but it was not found." format m)
  }
   
  def hasFractional[A](implicit f: Fractional[A] = null, m: Manifest[A]) {
    assert(f != null, "Expected implicit Fractional[%s] instance, but it was not found." format m)
  }

  def hasEuclideanRingWithNRoot[A](implicit e: EuclideanRingWithNRoot[A] = null, m: Manifest[A]) {
    assert(e != null, "Expected implicit EuclideanRingWithNRoot[%s] instance, but it was not found." format m)
  }
 
  def hasFieldWithNRoot[A](implicit e: FieldWithNRoot[A] = null, m: Manifest[A]) {
    assert(e != null, "Expected implicit FieldWithNRoot[%s] instance, but it was not found." format m)
  }
 
  def hasOrder[A](implicit ev: Order[A] = null, m: Manifest[A]) {
    assert(ev != null, "Expected implicit Order[%s] instance, but it was not found." format m)
  }
  
  def hasEq[A](implicit ev: Eq[A] = null, m: Manifest[A]) {
    assert(ev != null, "Expected implicit Eq[%s] instance, but it was not found." format m)
  }
 
  def hasConvertableFrom[A](implicit ev: ConvertableFrom[A] = null, m: Manifest[A]) {
    assert(ev != null, "Expected implicit ConvertableFrom[%s] instance, but it was not found." format m)
  }
  
  def hasConvertableTo[A](implicit ev: ConvertableTo[A] = null, m: Manifest[A]) {
    assert(ev != null, "Expected implicit ConvertableTo[%s] instance, but it was not found." format m)
  }

  def hasNRoot[A](implicit ev: NRoot[A] = null, m: Manifest[A]) {
    assert(ev != null, "Expected implicit NRoot[%s] instance, but it was not found." format m)
  }

  test("Numeric is ConvertableTo") {
    def check[A: Numeric : Manifest] {
      hasConvertableTo[A]
    }

    check[Int]
  }

  test("Numeric is ConvertableFrom") {
    def check[A: Numeric : Manifest] {
      hasConvertableFrom[A]
    }

    check[Int]
  }

  test("EuclideanRings are Rings") {
    def check[A: EuclideanRing: Manifest] {
      hasRing[A]
    }

    check[Int]
  }

  test("Fields are EuclideanRings") {
    def check[A: Field: Manifest] {
      hasEuclideanRing[A]
    }

    check[Double]
  }

  test("EuclideanRingWithNRoots are EuclideanRings") {
    def check[A: EuclideanRingWithNRoot: Manifest] {
      hasEuclideanRing[A]
    }

    check[BigInt]
  }

  test("FieldWithNRoots are Fields") {
    def check[A: FieldWithNRoot: Manifest] {
      hasField[A]
    }

    check[BigDecimal]
  }

  test("Numerics have Order, NRoot, are EuclideanRings and are Fields") {
    def check[A: Numeric: Manifest] {
      hasOrder[A]
      hasEuclideanRing[A]
      hasField[A]
      hasNRoot[A]
    }

    check[Int]
  }

  test("Fractional have Order, NRoot and are Fields") {
    def check[A: Fractional: Manifest] {
      hasOrder[A]
      hasEuclideanRing[A]
      hasField[A]
      hasNRoot[A]
    }

    check[Double]
  }

  test("Int is EuclideanRingWithNRoot") {
    hasEq[Int]
    hasRing[Int]
    hasEuclideanRing[Int]
    hasEuclideanRingWithNRoot[Int]
  }

  test("Long is EuclideanRingWithNRoot") {
    hasEq[Long]
    hasRing[Long]
    hasEuclideanRing[Long]
    hasEuclideanRingWithNRoot[Long]
  }

  test("BigBigInt is EuclideanRingWithNRoot") {
    hasEq[BigInt]
    hasRing[BigInt]
    hasEuclideanRing[BigInt]
    hasEuclideanRingWithNRoot[BigInt]
  }

  test("Float is FieldWithNRoot") {
    hasEq[Float]
    hasRing[Float]
    hasEuclideanRing[Float]
    hasField[Float]
    hasFieldWithNRoot[Float]
  }

  test("Double is FieldWithNRoot") {
    hasEq[Double]
    hasRing[Double]
    hasEuclideanRing[Double]
    hasField[Double]
    hasFieldWithNRoot[Double]
  }

  test("BigDecimal is FieldWithNRoot") {
    hasEq[BigDecimal]
    hasRing[BigDecimal]
    hasEuclideanRing[BigDecimal]
    hasField[BigDecimal]
    hasFieldWithNRoot[BigDecimal]
  }

  test("Rational is FieldWithNRoot") {
    implicit val ac = ApproximationContext(Rational(1, 100))
    hasEq[Rational]
    hasRing[Rational]
    hasEuclideanRing[Rational]
    hasField[Rational]
    hasFieldWithNRoot[Rational]
  }

  test("Real is FieldWithNRoot") {
    hasEq[Real]
    hasRing[Real]
    hasEuclideanRing[Real]
    hasField[Real]
    hasFieldWithNRoot[Real]
  }

  test("Everybody is Numeric") {
    implicit val ac = ApproximationContext(Rational(1, 100))

    hasNumeric[Int]
    hasNumeric[Long]
    hasNumeric[BigInt]
    hasNumeric[Float]
    hasNumeric[Double]
    hasNumeric[BigDecimal]
    hasNumeric[Rational]
    hasNumeric[Real]
  }

  test("Float, Double, Rational, BigDecimal, and Real are Fractional") {
    implicit val ac = ApproximationContext(Rational(1, 100))

    hasFractional[Float]
    hasFractional[Double]
    hasFractional[BigDecimal]
    hasFractional[Rational]
    hasFractional[Real]
  }

  test("FieldWithNRoot[Rational] requires implicit ApproximationContext") {
    def check[A](implicit f: FieldWithNRoot[A] = null) {
      assert(f == null)
    }

    check[Rational]
  }
}
 


package spire.matrix.dense.tests

import spire.matrix.dense.Matrix
import spire.matrix.Constants._

import org.scalatest.FunSuite

class MatrixTest extends FunSuite {

  test("Matrix elements") {
    val m = Matrix(2,3)(11, 12, 13,
      21, 22, 23)
    expectResult((2,3)) { m.dimensions }
    expectResult(6) { m.length }
    expectResult(11 :: 21 :: 12 :: 22 :: 13 :: 23 :: Nil) { m.toList }
    expectResult(11){ m(0,0) }
    expectResult(23){ m(1,2) }

    m.swap(0,1)(1,2)
    expectResult(Matrix(2,3)(11, 23, 13,
      21, 22, 12)) { m }
  }

  test("Matrix copy") {
    val a = Matrix(5,3)(1,   2,  3,
      4,   5,  6,
      7,   8,  9,
      10, 11, 12,
      13, 14, 15)
    val b = a.copyToMatrix
    assert(a == b)
    a(0,0) = -10
    assert(b(0,0) == 1)
  }

  test("Matrix columns") {
    val m = Matrix(5,3)( 1,  2,  3,
      4,  5,  6,
      7,  8,  9,
      10, 11, 12,
      13, 14, 15)
    expectResult(2 :: 5 :: 8 :: 11 :: 14 :: Nil) { m.column(1).toList }
    expectResult(5 :: 8 :: 11 :: 14 :: Nil) { m.column(1).block(1,End).toList }
    expectResult(2 :: 5 :: 8 :: Nil) { m.column(1).block(0,3).toList }
    expectResult(8 :: 11 :: Nil) { m.column(1).block(2,4).toList }
    expectResult(15 :: Nil) { m.column(2).block(4,End).toList }
    expectResult(Nil) { m.column(1).block(1,1).toList }
  }

  test("Search for trailing zero columns") {
    val m1 = Matrix(3,5)(11, 12, 13, 14, 15,
      21, 22, 23, 24, 25,
      31, 32, 33, 34, 35)
    expectResult(5){ m1.nonZeroColumnsEndIndex }
    val m2 = Matrix(3,5)(11, 12, 13, 14,  0,
      21, 22, 23, 24, 24,
      31, 32, 33, 34,  0)
    expectResult(5){ m2.nonZeroColumnsEndIndex }
    val m4 = Matrix(3,5)(11, 12, 13, 0, 0,
      21, 22, 23, 0, 0,
      31, 32, 33, 0, 0)
    expectResult(3){ m4.nonZeroColumnsEndIndex }
    val m3 = Matrix(3,5)(11, 12, 13, 14, 0,
      21, 22, 23, 24, 0,
      31, 32, 33, 34, 0)
    expectResult(4){ m3.nonZeroColumnsEndIndex }
    val m5 = Matrix(3,5)(0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0)
    expectResult(0){ m5.nonZeroColumnsEndIndex }
  }

  test("Search for trailing zero rows") {
    val m1 = Matrix(6,4)(11, 12, 13, 14,
      21, 22, 23, 24,
      31, 32, 33, 34,
      41, 42, 43, 44,
      51, 52, 53, 54,
      61, 62, 63, 64)
    expectResult(6){ m1.nonZeroRowsEndIndex }
    val m2 = Matrix(6,4)(11, 12, 13, 14,
      21, 22, 23, 24,
      31, 32, 33, 34,
      41, 42, 43, 44,
      51, 52, 53, 54,
      0, 62, 63,  0)
    expectResult(6){ m2.nonZeroRowsEndIndex }
    val m3 = Matrix(6,4)(11, 12, 13, 14,
      21, 22, 23, 24,
      31, 32, 33, 34,
      41, 42, 43, 44,
      51, 52, 53, 54,
      0,  0,  0,  0)
    expectResult(5){ m3.nonZeroRowsEndIndex }
    val m4 = Matrix(6,4)(11, 12, 13, 14,
      0,  0,  0,  0,
      0,  0,  0,  0,
      0,  0,  0,  0,
      0,  0,  0,  0,
      0,  0,  0,  0)
    expectResult(1){ m4.nonZeroRowsEndIndex }
    val m5 = Matrix(6,4)( 0,  0,  0,  0,
      0,  0,  0,  0,
      0,  0,  0,  0,
      0,  0,  0,  0,
      0,  0,  0,  0,
      0,  0,  0,  0)
    expectResult(0){ m5.nonZeroRowsEndIndex }
  }

  test("Matrix rows") {
    val m = Matrix(5,3)(1,   2,  3,
      4,   5,  6,
      7,   8,  9,
      10, 11, 12,
      13, 14, 15)
    expectResult(4 :: 5 :: 6 :: Nil) { m.row(1).toList }
    expectResult(13 :: 14 :: 15 :: Nil) { m.row(4).toList }
    val m1 = m.copyToMatrix
    m.row(3)(2) = -1
    m1(3,2) = -1
    expectResult(m1){ m }
    val r = m.row(0).block(1,3)
    expectResult(2) { r.length }
    r(1) = -2
    m1(0,2) = -2
    expectResult(m1){ m }
    val m2 = Matrix(6,4)(11, 12, 13, 14,
      21, 22, 23, 24,
      31, 32, 33, 34,
      41, 42, 43, 44,
      51, 52, 53, 54,
      61, 62, 63, 64)
    expectResult(51::52::53::54::Nil) { m2.row(4).block(0,End) }
    expectResult(31 :: 32 :: Nil) { m2.row(2).block(0,2) }
  }

  test("1-norm of 2 x 3 matrix") {
    val m = Matrix(2,3)(11, 12, -13,
      21, 22,  23)
    expectResult(36) { m.norm1 }
  }

  test("1-norm of 3 x 2 matrix") {
    val m = Matrix(3,2)(11,  12,
      21,  22,
      31, -32)
    expectResult(66) { m.norm1 }
  }

  test("1-norm of 3 x 3 matrix") {
    val m = Matrix(3,3)(-11, 12, 13,
      21, 22, 23,
      31, 32, 33)
    expectResult(69) { m.norm1 }
  }

  test("infinity-norm of 2 x 3 matrix") {
    val m = Matrix(2,3)(11, 12, -13,
      21, 22,  23)
    expectResult(66) { m.normInf }
  }

  test("infinity-norm of 3 x 2 matrix") {
    val m = Matrix(3,2)(11,  12,
      21,  22,
      31, -32)
    expectResult(63) { m.normInf }
  }

  test("infinity-norm of 3 x 3 matrix") {
    val m = Matrix(3,3)(-11, 12, 13,
      21, 22, 23,
      31, 32, 33)
    expectResult(96) { m.normInf }
  }

  test("frobenius norm") {
    val m = Matrix(2,2)(1, 2,
      2, 4)
    expectResult(5) { m.normFrobenius }
  }

  test("diagonal") {
    val m = Matrix(3,5)(11, 12, 13, 14, 15,
                        21, 22, 23, 24, 25,
                        31, 32, 33, 34, 35)
    expectResult(31 :: Nil) { m.diagonalOfOrder(-2).toList }
    expectResult(21 :: 32 :: Nil) { m.diagonalOfOrder(-1).toList }
    expectResult(11 :: 22 :: 33 :: Nil) { m.diagonal.toList }
    expectResult(12 :: 23 :: 34 :: Nil) { m.diagonalOfOrder(1).toList }
    expectResult(13 :: 24 :: 35 :: Nil) { m.diagonalOfOrder(2).toList }
    expectResult(14 :: 25 :: Nil) { m.diagonalOfOrder(3).toList }
    expectResult(15 :: Nil) { m.diagonalOfOrder(4).toList }

    val m1 = Matrix(5,4)(11, 12, 13, 14,
                         21, 22, 23, 24,
                         31, 32, 33, 34,
                         41, 42, 43, 44,
                         51, 52, 53, 54)
    expectResult(51 :: Nil) { m1.diagonalOfOrder(-4).toList }
    expectResult(41 :: 52 :: Nil) { m1.diagonalOfOrder(-3).toList }
    expectResult(31 :: 42 :: 53 :: Nil) { m1.diagonalOfOrder(-2).toList }
    expectResult(21 :: 32 :: 43 :: 54 :: Nil) { m1.diagonalOfOrder(-1).toList }
    expectResult(11 :: 22 :: 33 :: 44 :: Nil) { m1.diagonalOfOrder(0).toList }
    expectResult(12 :: 23 :: 34 :: Nil) { m1.diagonalOfOrder(1).toList }
    expectResult(13 :: 24 :: Nil) { m1.diagonalOfOrder(2).toList }
    expectResult(14 :: Nil) { m1.diagonalOfOrder(3).toList }

    val m2 = Matrix(3,3)(11, 12, 13,
                         21, 22, 23,
                         31, 32, 33)
    expectResult(31 :: Nil) { m2.diagonalOfOrder(-2).toList }
    expectResult(21 :: 32 :: Nil) { m2.diagonalOfOrder(-1).toList }
    expectResult(11 :: 22 :: 33 :: Nil) { m2.diagonalOfOrder(0).toList }
    expectResult(12 :: 23 :: Nil) { m2.diagonalOfOrder(1).toList }
    expectResult(13 :: Nil) { m2.diagonalOfOrder(2).toList }
  }

  test("string conversion") {
    val a = Matrix(2,3)(1, 2, 3,
      4, 5, 6)
    expectResult("""|
                    |[       1.00        2.00        3.00 ]
                    |[       4.00        5.00        6.00 ]
                    |"""
      .stripMargin)
    {
      a.toString
    }

  }

  test("iteration over matrix block") {
    val a = Matrix(5,7)( 1,  2,  3,  4,  5,  6,  7,
      8,  9, 10, 11, 12, 13, 14,
      15, 16, 17, 18, 19, 20, 21,
      22, 23, 24, 25, 26, 27, 28,
      29, 30, 31, 32, 33, 34, 35)
    val b1 = a.block(2,4)(1,4)
    expectResult(25){ b1(1,2) }
    expectResult(16 :: 23 :: 17 :: 24 :: 18 :: 25 :: Nil) { b1.toList }
    val b2 = a.block(1,5)(3,4)
    expectResult(11 :: 18 :: 25 :: 32 :: Nil) { b2.toList }
  }

  test("element access for matrix block") {
    val a = Matrix(3,4)(11, 12, 13, 14,
      21, 22, 23, 24,
      31, 32, 33, 34)
    val b = a.block(1,3)(1,4)
    expectResult(22) { b(0,0) }
    expectResult(23) { b(0,1) }
    expectResult(24) { b(0,2) }
    expectResult(32) { b(1,0) }
    expectResult(33) { b(1,1) }
    expectResult(34) { b(1,2) }

    expectResult(22) { b(0)}
    expectResult(32) { b(1)}
    expectResult(23) { b(2)}
    expectResult(33) { b(3)}
    expectResult(24) { b(4)}
  }
}

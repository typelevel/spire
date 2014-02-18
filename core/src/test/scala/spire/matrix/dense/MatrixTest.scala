package spire.matrix.dense.tests

import spire.matrix.dense.{Vector, Matrix, Permutation}
import spire.syntax.cfor._

import org.scalatest.FunSuite

class MatrixTest extends FunSuite {

  val V = Vector

  test("Matrix elements") {
    val m = Matrix(2,3)(11, 12, 13,
                        21, 22, 23)
    expectResult((2,3)) { m.dimensions }
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
    val (m,n) = (5,3)
    val a = Matrix(5,3)( 1,  2,  3,
                         4,  5,  6,
                         7,  8,  9,
                         10, 11, 12,
                         13, 14, 15)
    expectResult(Vector(2 , 5 , 8 , 11 , 14)) { a.column(1) }
    expectResult(Vector(5 , 8 , 11 , 14)) { a.column(1).block(1,m) }
    expectResult(Vector(2 , 5 , 8)) { a.column(1).block(0,3) }
    expectResult(Vector(8 , 11)) { a.column(1).block(2,4) }
    expectResult(Vector(15)) { a.column(2).block(4,m) }
    expectResult(Vector()) { a.column(1).block(1,1) }
  }

  test("Block of a matrix column") {
    val (m,n) = (5,3)
    val a = Matrix(5,3)( 1,  2,  3,
                         4,  5,  6,
                         7,  8,  9,
                         10, 11, 12,
                         13, 14, 15)
    expectResult { Vector(7 , 10 , 13) } { a.column(0).block(2,5) }
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
    val a = Matrix(5,3)(1,   2,  3,
                        4,   5,  6,
                        7,   8,  9,
                        10, 11, 12,
                        13, 14, 15)
    expectResult(Vector(4 , 5 , 6)) { a.row(1) }
    expectResult(Vector(13 , 14 , 15)) { a.row(4) }
    val a1 = a.copyToMatrix
    a.row(3)(2) = -1
    a1(3,2) = -1
    expectResult(a1){ a }
    val r = a.row(0).block(1,3)
    expectResult(2) { r.dimension }
    r(1) = -2
    a1(0,2) = -2
    expectResult(a1){ a }
    val a2 = Matrix(6,4)(11, 12, 13, 14,
                         21, 22, 23, 24,
                         31, 32, 33, 34,
                         41, 42, 43, 44,
                         51, 52, 53, 54,
                         61, 62, 63, 64)
    expectResult(Vector(51,52,53,54)) { a2.row(4).block(0,4) }
    expectResult(Vector(31 , 32)) { a2.row(2).block(0,2) }
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
    expectResult(Vector(31)) { m.diagonalOfOrder(-2) }
    expectResult(Vector(21 , 32)) { m.diagonalOfOrder(-1) }
    expectResult(Vector(11 , 22 , 33)) { m.diagonal }
    expectResult(Vector(12 , 23 , 34)) { m.diagonalOfOrder(1) }
    expectResult(Vector(13 , 24 , 35)) { m.diagonalOfOrder(2) }
    expectResult(Vector(14 , 25)) { m.diagonalOfOrder(3) }
    expectResult(Vector(15)) { m.diagonalOfOrder(4) }

    val m1 = Matrix(5,4)(11, 12, 13, 14,
                         21, 22, 23, 24,
                         31, 32, 33, 34,
                         41, 42, 43, 44,
                         51, 52, 53, 54)
    expectResult(Vector(51)) { m1.diagonalOfOrder(-4) }
    expectResult(Vector(41 , 52)) { m1.diagonalOfOrder(-3) }
    expectResult(Vector(31 , 42 , 53)) { m1.diagonalOfOrder(-2) }
    expectResult(Vector(21 , 32 , 43 , 54)) { m1.diagonalOfOrder(-1) }
    expectResult(Vector(11 , 22 , 33 , 44)) { m1.diagonalOfOrder(0) }
    expectResult(Vector(12 , 23 , 34)) { m1.diagonalOfOrder(1) }
    expectResult(Vector(13 , 24)) { m1.diagonalOfOrder(2) }
    expectResult(Vector(14)) { m1.diagonalOfOrder(3) }

    val m2 = Matrix(3,3)(11, 12, 13,
                         21, 22, 23,
                         31, 32, 33)
    expectResult(Vector(31)) { m2.diagonalOfOrder(-2) }
    expectResult(Vector(21 , 32)) { m2.diagonalOfOrder(-1) }
    expectResult(Vector(11 , 22 , 33)) { m2.diagonalOfOrder(0) }
    expectResult(Vector(12 , 23)) { m2.diagonalOfOrder(1) }
    expectResult(Vector(13)) { m2.diagonalOfOrder(2) }
  }

  test("string conversion") {
    val a = Matrix(2,3)(1, 2, 3,
                        4, 5, 6)
    expectResult("""|
                    |[       1.00       2.00       3.00 ]
                    |[       4.00       5.00       6.00 ]
                    |"""
      .stripMargin)
    {
      a.formatted("%10.3g")
    }

    expectResult("""|
                    |{{      1.00,      2.00,      3.00},
                    |{      4.00,      5.00,      6.00}}
                    |"""
      .stripMargin)
    {
      a.formatted("%10.3g", useMathematicaFormat=true)
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
  }

  test("columns of matrix block") {
    val a = Matrix(4,3)(11, 12, 13,
                        21, 22, 23,
                        31, 32, 33,
                        41, 42, 43)
    val b = a.block(1,4)(1,3)
    expectResult(Vector(22 , 32 , 42)) { b.column(0) }
    expectResult(Vector(23 , 33 , 43)) { b.column(1) }
  }

  test("block of matrix block") {
    val a = Matrix(5,5)(11, 12, 13, 14, 15,
                        21, 22, 23, 24, 25,
                        31, 32, 33, 34, 35,
                        41, 42, 43, 44, 45,
                        51, 52, 53, 54, 55)
    val b = a.block(1,5)(2,5).block(1,4)(1,3)
    expectResult { b(0,1) } { 35 }
    expectResult { 34 :: 44 :: 54 :: 35 :: 45 :: 55 :: Nil } { b.toList }
  }

  test("Permutation of matrix rows") {
    val a0 = Matrix(6,3)(11, 12, 13,
                         21, 22, 23,
                         31, 32, 33,
                         41, 42, 43,
                         51, 52, 53,
                         61, 62, 63)
    val p = Permutation(1, 1, 4, 0, 4, 5)

    val a1 = a0.copyToMatrix
    p.permute_rows(a1)
    expectResult {
      Matrix(6,3)(
        41, 42, 43,
        11, 12, 13,
        51, 52, 53,
        21, 22, 23,
        31, 32, 33,
        61, 62, 63
      )
    } { a1 }

    val a2 = a0.copyToMatrix
    p.inverse.permute_rows(a2)
    expectResult {
      Matrix(6,3)(
        21, 22, 23,
        41, 42, 43,
        51, 52, 53,
        11, 12, 13,
        31, 32, 33,
        61, 62, 63
      )
    } { a2 }

    val b0 = Matrix.tabulate(3,8)((i,j) => 10*(i+1) + (j+1))
    val q = Permutation(2, 1, 2)
    val b = b0.copyToMatrix
    q.permute_rows(b, blockSize=3)
    cforRange(0 until 8) { j => b0.swap(0,j)(2,j) }
    expectResult(b0)(b)

    val c0 = Matrix(3,3)(1.0, 0.0, 0.0,
                         0.0, 0.7, 0.0,
                         0.0, 0.0, 0.5)
    val c = c0.copyToMatrix
    val r = Permutation(0, 2, 1)
    r.permute_rows(c)
    expectResult { c0 } { c }
  }

  test("Permutation of a subset of matrix rows") {
    val a0 = Matrix.tabulate(5,2)((i,j) => 10*(i+1) + (j+1))
    val p = Permutation.identity(5)
    val q = p.restrictTo(1,4)
    q(0) = 1
    q(1) = 2
    q(2) = 2
    expectResult { 1 } { q(0) }
    expectResult { 2 } { q(1) }
    expectResult { 2 } { q(2) }
    expectResult { 0 :: 2 :: 3 :: 3 :: 4 :: Nil } { p.toList }

    val a = a0.copyToMatrix
    q.permute_rows(a.block(1,4)(0,2))
    expectResult { a0.row(0) } { a.row(0) }
    expectResult { Matrix(3,2)(31, 32,
                               41, 42,
                               21, 22) } { a.block(1,4)(0,2) }
    expectResult { a0.row(4) } { a.row(4) }
  }
}

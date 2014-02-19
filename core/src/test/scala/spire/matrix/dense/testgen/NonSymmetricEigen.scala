package spire.matrix.dense.tests

import spire.matrix.dense._
import spire.matrix.{Transposition, UpperOrLower, Sides, DiagonalProperty}
import Transposition._
import UpperOrLower._
import Sides._
import DiagonalProperty._
import spire.matrix.NumericPropertiesOfDouble
import spire.matrix.dense.BLAS
import spire.matrix.dense.random._
import spire.implicits._
import scala.math.{sqrt, min, max}

/**
 * Special diagonal construction mode
 *
 * Their id ranging from 1 to 5 corresponds to LAPACK integer values
 */
object SpecialDiagonalMode extends Enumeration(1) {
  val ClusteredSmall, ClusteredLarge, Geometric, Arithmetic, Random = Value
  val Other = Value
}

/**
 * Special diagonal constructed according to given specs.
 *
 * Given the mode and a condition number c >= 1:
 *
 *  - mode = ClusteredSmall sets d(0) = 1 and d(1:n) = 1/c
 *  - mode = ClusteredLarge sets d(0:n-1) = 1 and d(n-1) = 1/c
 *  - mode = Geometric sets d(i) = c**(-i/(n-1)),
 *    i.e. geometric progression from 1 to 1/c
 *  - mode = Arithmetic sets d(i) = 1 - i/(n-1)(1 - 1/c)
 *    i.e. arithmetic progression from 1 to 1/c
 *  - mode = Random sets d to random numbers in the range (1/c, 1) such that
 *    their logarithm are uniformly distributed.
 *
 * Reference: subroutine DLATM1 of [1]: we follow very closely this subroutine
 * and some of its documentation is copied verbatim; we do not implement
 * either mode=0 or mode=6 as those different cases of diagonals will be
 * handled at a higher level.
 *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
class SpecialDiagonal(n:Int, mode:SpecialDiagonalMode.Value, c:Double,
                      reverse:Boolean=false,
                      signs:BernoulliDistribution=null,
                      uniform01:ScalarUniformDistributionFromZeroToOne=null)
extends Vector(n) with VectorMagnitudeLimitation
{
  import SpecialDiagonalMode._
  require(mode != Random || uniform01 != null)
  require(c >= 1)
  mode match {
    case ClusteredSmall => this(0)           = 1; this.block(1, n) := 1.0/c
    case ClusteredLarge => this.block(1, n) := 1; this(0)           = 1.0/c
    case Geometric => {
      this(0) = 1
      cforRange(1 to n-1) { i =>
        this(i) = c**(-i.toDouble/(n-1))
      }
    }
    case Arithmetic => {
      this(0) = 1
      cforRange(1 to n-1) { i =>
        this(i) = 1 - i.toDouble/(n-1)*(1-1.0/c)
      }
    }
    case Random => cforRange(0 until n) { i =>
      this(i) = (1.0/c)**uniform01.next
    }
  }
  if(reverse) reverseInPlace
  if(signs != null) cforRange(0 until n) { i =>
    if(signs.next) this(i) = -this(i)
  }
}


/**
 * Non-symmetric square matrices with methods to tailor their eigenvalues
 *
 * Reference: [1] and subroutine DLATME from [2]
 *
 * [1] James W. Demmel and Alan McKenney,
 *     A test matrix generation suite,
 *     Tech. Report 9, LAPACK Working Note,
 *     March 1989.
 *
 * [2] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
class GeneralSquareMatrix(n:Int, a:Array[Double])
extends Matrix(n, n, n, 0, a)
with NumericPropertiesOfDouble with MatrixMagnitudeLimitation {

  /**
   * Jordan block
   *
   * with 1's on the diagonal, transposed, i.e. subdiagonal made of 1's
   * instead of superdiagonal of 1's as is more traditional
   */
  def makeJordanBlock: this.type = {
    cforRange(0 until n) { j =>
      this(j, j) = 1.0
      if(j > 0) this(j, j-1) = 1.0
    }
    this
  }

  /**
   * Each even-odd pair of diagonal elements will be either used as two real
   * eigenvalues or as the real and imaginary part of a complex conjugate pair
   * of eigenvalues; the choice of which is done is specified by the
   * sequence of Char's produced by `setup`:
   *
   * - "... RR ..." means two real eigenvalues whereas
   * - "... RI ..." means two complex-conjugate ones
   *
   * whereas "... IR ..." or "... II ..." would be a faulty input.
   */
  def makeComplexConjugateEigenvaluePairs(setup:Iterator[Char]): this.type =
  {
    var previous:Char = ' '
    cforRange(0 until n) { j =>
      val current = setup.next
      if(current == 'I') {
        if(previous == ' ')
          throw new RuntimeException("First instruction shall be 'R'")
        else if(previous == 'I')
          throw new RuntimeException(
            s"Instruction 'I' at index $j shall be preceded by 'R'")
        /*
             [a   0]   ==>  [ a  b] whose eigenvalues are a +/- ib
             [0   b]        [-b  a]
         */
        val (a, b) = (this(j-1, j-1), this(j, j))
        this(j-1, j  ) =  b
        this(j  , j-1) = -b
        this(j  , j  ) =  a
      }
      previous = current
    }
    this
  }

  /** Overload taking a Char sequence of a different type */
  def makeComplexConjugateEigenvaluePairs(setup:Iterable[Char]): this.type = {
    makeComplexConjugateEigenvaluePairs(setup.iterator)
    this
  }

  /**
   * Overload taking a random generator to create the sequence of R's and I's
   *
   * Each even-odd pair is either "RR" or "RI" and the probability of the
   * former is that of true for the given Bernoulli distribution.
   */
  def makeComplexConjugateEigenvaluePairs(coin:BernoulliDistribution): this.type =
  {
    makeComplexConjugateEigenvaluePairs(
      Iterator.single('R')
      ++ coin.flatMap(if(_) Iterator('R', 'R') else Iterator('R', 'I')))
    this
  }

  /**
   * Set upper triangle to random numbers
   *
   * but don't modify the corners of 2x2 diagonal blocks that may have
   * been created by `makeComplexConjugateEigenvaluePairs`
   */
  def makeRandomUpperDiagonal(elements:ScalarDistribution): this.type = {
    cforRange(1 until n) { j =>
      val iEnd = if(this(j-1, j) != 0.0) j-1 else j
      cforRange(0 until iEnd) { i => this(i, j) = elements.next }
    }
    this
  }

  /**
   * Apply random similarity transformation
   *
   * The transform reads X A X^-1^ where X = U S V,
   * where U and V are random orthogonal matrices.
   * The arguments `mode`, `c`, `reverse`, and  `uniform01`
   * are used to construct S, c.f. class SpecialDiagonal.
   */
  def applyRandomSimilarityTransform(
    mode:SpecialDiagonalMode.Value,
    c:Double,
    haar:OrthogonalMatricesHaarDistribution,
    uniform01:ScalarUniformDistributionFromZeroToOne=null)
  : this.type = {
    val d = new SpecialDiagonal(n, mode, c, signs=null, uniform01=uniform01)

    // Denoting this by A, do A := V A V^T
    haar.overwriteWithProductByNext(Congruent, this)

    // A := S A S^{-1}
    cforRange(0 until n) { j =>
      assert(d(j) != 0)
      scale(d(j), row(j))
      scale(1/d(j), column(j))
    }

    // A := U A U^T
    haar.overwriteWithProductByNext(Congruent, this)

    this
  }

  def applyRandomOrthogonalSimilarityTransform(
    haar:OrthogonalMatricesHaarDistribution)
  : this.type = {
    haar.overwriteWithProductByNext(Congruent, this)
    this
  }

  // TODO: bandwidth reduction
}

/**
 * Companion object of class spire.matrix.dense.test.SquareMatrix
 */
object GeneralSquareMatrix
extends MatrixConstruction[GeneralSquareMatrix] with NumericPropertiesOfDouble {

  def empty(m:Int, n:Int): GeneralSquareMatrix = zero(m, n)

  def apply(m:Int, n:Int, elements:Array[Double]) = {
    require(m == n)
    new GeneralSquareMatrix(n, elements)
  }

}

/**
 * Samples of matrices tailored to test algorithms used in eigen decomposition
 *
 * It is actually a sample of pairs (type, m) where `type` is an integer that
 * summarises the properties of the test matrix: it corresponds to argument
 * ITYPE of subroutine DCHKHS in [2] (c.f. also [1] for a description of that
 * testing framework). The list of possible types (copying DCHKHS documentation
 * verbatim) is:
 *
 *    1.  The zero matrix.
 *    2.  The identity matrix.
 *    3.  A (transposed) Jordan block, with 1's on the diagonal.
 *
 *    4.  A diagonal matrix with evenly spaced entries
 *        1, ..., ULP  and random signs.
 *        (ULP = (first number larger than 1) - 1 )
 *    5.  A diagonal matrix with geometrically spaced entries
 *        1, ..., ULP  and random signs.
 *    6.  A diagonal matrix with "clustered" entries 1, ULP, ..., ULP
 *        and random signs.
 *
 *    7.  Same as (4), but multiplied by SQRT( overflow threshold )
 *    8.  Same as (4), but multiplied by SQRT( underflow threshold )
 *
 *    9.  A matrix of the form  U' T U, where U is orthogonal and
 *        T has evenly spaced entries 1, ..., ULP with random signs
 *        on the diagonal and random O(1) entries in the upper
 *        triangle.
 *
 *    10. A matrix of the form  U' T U, where U is orthogonal and
 *        T has geometrically spaced entries 1, ..., ULP with random
 *        signs on the diagonal and random O(1) entries in the upper
 *        triangle.
 *
 *    11. A matrix of the form  U' T U, where U is orthogonal and
 *        T has "clustered" entries 1, ULP,..., ULP with random
 *        signs on the diagonal and random O(1) entries in the upper
 *        triangle.
 *
 *    12. A matrix of the form  U' T U, where U is orthogonal and
 *        T has real or complex conjugate paired eigenvalues randomly
 *        chosen from ( ULP, 1 ) and random O(1) entries in the upper
 *        triangle.
 *
 *    13. A matrix of the form  X' T X, where X has condition
 *        SQRT( ULP ) and T has evenly spaced entries 1, ..., ULP
 *        with random signs on the diagonal and random O(1) entries
 *        in the upper triangle.
 *
 *    14. A matrix of the form  X' T X, where X has condition
 *        SQRT( ULP ) and T has geometrically spaced entries
 *        1, ..., ULP with random signs on the diagonal and random
 *        O(1) entries in the upper triangle.
 *
 *    15. A matrix of the form  X' T X, where X has condition
 *        SQRT( ULP ) and T has "clustered" entries 1, ULP,..., ULP
 *        with random signs on the diagonal and random O(1) entries
 *        in the upper triangle.
 *
 *    16. A matrix of the form  X' T X, where X has condition
 *        SQRT( ULP ) and T has real or complex conjugate paired
 *        eigenvalues randomly chosen from ( ULP, 1 ) and random
 *        O(1) entries in the upper triangle.
 *
 *    17. Same as (16), but multiplied by SQRT( overflow threshold )
 *    18. Same as (16), but multiplied by SQRT( underflow threshold )
 *
 *    19. Nonsymmetric matrix with random entries chosen from (-1,1).
 *    20. Same as (19), but multiplied by SQRT( overflow threshold )
 *    21. Same as (19), but multiplied by SQRT( underflow
 *
 * References:
 *
 * [1] James W. Demmel and Alan McKenney,
 *     A test matrix generation suite,
 *     Tech. Report 9, LAPACK Working Note,
 *     March 1989.
 *
 * [2] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
class EigenTestMatrices(nonSpecialDimensions:Int = 0,
                        randomMatricesOfEachType:Int = 1,
                        excludedTypes:Set[Int] = Set(),
                        includedTypes:Set[Int] = Set(1 to 21:_*))
                       (implicit gen:Defaults.IntegerGenerator)
extends TestDimensions with NumericPropertiesOfDouble
{
  val ulp = precision

  val uniformI01 = new ScalarUniformDistributionFromZeroToOne
  val uniformIm1p1 = new ScalarUniformDistributionFromMinusOneToOne
  val coin = new BernoulliDistribution(0.5)

  val types = includedTypes -- excludedTypes
  val typesWithArithmeticDiagonal = types & Set(4, 7, 8, 9, 13)
  val typesWithGeometicDiagonal = types & Set(5, 10, 14)
  val typesWithClusteredDiagonal = types & Set(6, 11, 15)
  val typesWithRandomSchurBlockDiagonal = types & Set(12, 16, 17, 18)
  val typesWithUpperDiagonal = types & Set(9 to 18: _*)
  val typesWithOrthogonalSimilarity = types & Set(9 to 12: _*)
  val typesWithIllConditionedSimilarity = types & Set(13 to 18: _*)
  val typesWithLargeMagnitude = types & Set(7, 17, 20)
  val typesWithSmallMagnitude = types & Set(8, 18, 21)

  // Lifted from DCHKHS
  // Note the correction for the matrix size.
  def sqrtOverflowThreshold(n:Int) = sqrt(overflow)*n*(1.0/ulp)
  def sqrtUnderflowThreshold(n:Int) = ( sqrt(safeMinimum)*ulp )*1.0/n

  /**
   * A sample of matrices of size n x n
   *
   */
  def sample(n:Int)(
    implicit work:Scratchpad = new Scratchpad(
      OrthogonalMatricesHaarDistribution.minimumScratchpad(n))) =
  new Traversable[(Int, Matrix)] {

    val orthogonal = new OrthogonalMatricesHaarDistribution(n)

    def foreach[U](f: ((Int, Matrix)) => U): Unit = {
      // deterministic types 1, 2, and 3: produce only once
      for(itype <- 1 to 3 if types.contains(itype)) {
        val a = itype match {
          case 1 => Matrix.zero(n)
          case 2 => Matrix.identity(n)
          case 3 => GeneralSquareMatrix.zero(n).makeJordanBlock
        }
        // yield
        f(itype, a)
      }

      // random types: produce the specified number for each
      for(cnt <- 1 to randomMatricesOfEachType) {
        // types where the matrix is constructed starting from a diagonal matrix
        for(itype <- 4 to 18 if types.contains(itype)) {
          // Setup diagonal
          val mode:SpecialDiagonalMode.Value =
            if(typesWithArithmeticDiagonal.contains(itype))
              SpecialDiagonalMode.Arithmetic
            else if(typesWithGeometicDiagonal.contains(itype))
              SpecialDiagonalMode.Geometric
            else if(typesWithClusteredDiagonal.contains(itype))
              SpecialDiagonalMode.ClusteredSmall
            else if(typesWithRandomSchurBlockDiagonal.contains(itype))
              SpecialDiagonalMode.Random
            else SpecialDiagonalMode.Other
          assert(mode != SpecialDiagonalMode.Other,
                 s"""|Invalid mode of construction of the diagonal
                     |for matrix type ($itype)""")
          val a = GeneralSquareMatrix.zero(n)
          a.diagonal := new SpecialDiagonal(n, mode, 1.0/ulp, false,
                                            coin, uniformI01)

          // Create Schur blocks if requested
          if(typesWithRandomSchurBlockDiagonal.contains(itype))
            a.makeComplexConjugateEigenvaluePairs(coin)

          // Fill upper diagonal if requested
          if(typesWithUpperDiagonal.contains(itype))
            a.makeRandomUpperDiagonal(uniformIm1p1)

          // Apply similarity if requested
          if(typesWithOrthogonalSimilarity.contains(itype)) {
            a.makeRandomUpperDiagonal(uniformIm1p1)
            a.applyRandomOrthogonalSimilarityTransform(orthogonal)
          }
          else if(typesWithIllConditionedSimilarity.contains(itype)) {
            a.makeRandomUpperDiagonal(uniformIm1p1)
            a.applyRandomSimilarityTransform(mode, 1.0/sqrt(ulp),
                                             orthogonal, uniformI01)
          }

          // Rescale element magnitudes if requested
          if(typesWithSmallMagnitude.contains(itype))
            a.rescaleElementMagnitudeTo(sqrtUnderflowThreshold(n))
          else if(typesWithLargeMagnitude.contains(itype))
            a.rescaleElementMagnitudeTo(sqrtOverflowThreshold(n))

          // yield
          f(itype, a)
        }

        // Matrices with random elements in the range (-1, 1)
        for(itype <- 19 to 21 if types.contains(itype)) {
          val a = GeneralSquareMatrix.fill(n,n)(uniformIm1p1.next)

          // Rescale element magnitudes if requested
          if(typesWithSmallMagnitude.contains(itype))
            a.rescaleElementMagnitudeTo(sqrtUnderflowThreshold(n))
          else if(typesWithLargeMagnitude.contains(itype))
            a.rescaleElementMagnitudeTo(sqrtOverflowThreshold(n))

          // yield
          f(itype, a)
        }
      }
    }
  }

  /** A sample of matrix of various sizes */
  def sample: Traversable[(Int, Matrix)] = new Traversable[(Int, Matrix)] {
    def foreach[U](f: ((Int, Matrix)) => U): Unit = {
      for(n <- oneDimensionSample) {
        for((itype, a) <- sample(n)) f(itype, a)
      }
    }
  }
}

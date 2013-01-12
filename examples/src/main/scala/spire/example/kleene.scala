package spire.example

import Predef.{any2stringadd => _, _}

import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.reflect.ClassTag
import scala.annotation.tailrec

/**
 * This example is taken from http://r6.ca/blog/20110808T035622Z.html.
 *
 * The goal is to try to do as direct a translation as possible from the
 * Haskell, to see how well we can do with Spire.
 *
 * The original example is in literate Haskell with good comments, so consult
 * the link for more information.
 */
object KleeneExample {
  import Aux._

  /**
   * A StarRig (aka Star-Semiring) is a Rig with asteration (kstar).
   * 
   * Laws:
   * 1. kstar(a) = one + a * kstar(a) = one + kstar(a) * a
   */
  trait StarRig[A] extends Rig[A] {
    // one of these must be overridden
    def kstar(a: A): A = plus(one, kplus(a))
    def kplus(a: A): A = times(a, kstar(a))
  }

  object StarRig {
    final def apply[A](implicit ev: StarRig[A]) = ev
  }

  /**
   * A KleenAlgebra is a StarRig with additional invariants.
   *
   * Laws:
   * 1. a + a = a
   * 2. a * x + x = x  ==> kstar(a) * x + x = x
   * 3. x * a + x = x  ==>  x * kstar(a) + x = x
   */
  trait KleeneAlgebra[A] extends StarRig[A]

  object KleeneAlgebra {
    final def apply[A](implicit ev: KleeneAlgebra[A]) = ev
  }

  /**
   * Type class implementation of KleeneAlgebra for Matrix[A].
   *
   * Works for any square matrix provided KleeneAlgebra[A] exists.
   */
  class MatrixHasKleeneAlgebra[A](implicit dim: Dim, ka: KleeneAlgebra[A], ct: ClassTag[A])
      extends KleeneAlgebra[Matrix[A]] {

    def zero: Matrix[A] = Matrix.zero
    def one: Matrix[A] = Matrix.one
    def plus(x: Matrix[A], y: Matrix[A]) = x + y
    def times(x: Matrix[A], y: Matrix[A]) = x * y

    override def kplus(x: Matrix[A]) = {
      def f(k: Int, m: Matrix[A]) = Matrix[A] { (x, y) =>
        m(x, y) + m(x, k) * StarRig[A].kstar(m(k, k)) * m(k, y)
      }

      @tailrec def loop(m: Matrix[A], i: Int): Matrix[A] =
        if (i < dim.n) loop(f(i, m), i + 1) else m

      loop(x, 0)
    }
  }

  implicit def matrixHasKleeneAlgebra[A](implicit dim: Dim, ka: KleeneAlgebra[A], ct: ClassTag[A]) =
    new MatrixHasKleeneAlgebra[A]

  /**
   * Type class implementation of KleeneAlgebra for Boolean.
   */
  implicit object BooleanHasKleeneAlgebra extends KleeneAlgebra[Boolean] {
    def zero = false
    def one = true
    def plus(x: Boolean, y: Boolean) = x || y
    def times(x: Boolean, y: Boolean) = x && y
    override def kstar(x: Boolean) = true
  }

  def main(args: Array[String]) {
    // our example graph will be 5x5
    implicit val dim = Dim(5)

    // build our example graph from a sequence of directional edges
    val example: Graph = Graph((0, 1), (1, 2), (2, 3), (2, 4), (3, 1), (4, 3))

    // examine the graph
    println("adjacency:")
    printMatrix(example)
    println()
    println("reflexive-transitive closure:")
    printMatrix(KleeneAlgebra[Graph].kstar(example))
    println()
    println("transitive closure:")
    printMatrix(KleeneAlgebra[Graph].kplus(example))
    println()
  }
}

/**
 * Auxilliary code, such as the toy matrix implementation.
 *
 * This code is split out to try to avoid distracting from the algebra part.
 */
object Aux {
  /**
   * Dim is a cute little class that let's us have implicit size information.
   * 
   * This is to work around the fact that we don't currently have
   * implementations of Bounded[A] or Ix[A] like Haskell does.
   *
   * Dim is probably not robust enough for real world use.
   */
  case class Dim(n: Int)

  /**
   * Naive matrix trait.
   */
  trait Matrix[A] { lhs =>
    def dim: Dim
    def apply(x: Int, y: Int): A
    def map[B: Rig: ClassTag](f: A => B): Matrix[B]
    def +(rhs: Matrix[A]): Matrix[A]
    def *(rhs: Matrix[A]): Matrix[A]
  }

  object Matrix {
    /**
     * Builds a Matrix[A] given a function (Int, Int) => A and an implicit Dim
     * to provide the dimensions over which to run the function.
     */
    def apply[A: Rig: ClassTag](f: (Int, Int) => A)(implicit dim: Dim): Matrix[A] = {
      val n = dim.n
      val arr = new Array[A](n * n)
      cfor(0)(_ < n, _ + 1) { y =>
        cfor(0)(_ < n, _ + 1) { x =>
          arr(y * n + x) = f(x, y)
        }
      }
      new ArrayMatrix(arr)
    }

    /**
     * Given an implicit Dim, builds the zero matrix (all zeros).
     */
    def zero[A: Rig: ClassTag](implicit dim: Dim): Matrix[A] =
      apply((x, y) => Rig[A].zero)

    /**
     * Given an implicit Dim, builds the identity matrix (diagonal ones).
     */
    def one[A: Rig: ClassTag](implicit dim: Dim): Matrix[A] =
      apply((x, y) => if (x == y) Rig[A].one else Rig[A].zero)
  }

  /**
   * Mutable ArrayMatrix implementation.
   * 
   * The mutability should only be used to initialize a matrix. Once it's built
   * it will be typed as Matrix[A] with no interface for further mutation.
   * 
   * The matrix also has naive implementations of addition and multiplication.
   * These are not optimized--do not use this class in the wild!
   */
  case class ArrayMatrix[A](arr: Array[A])(implicit val dim: Dim, rig: Rig[A], ct: ClassTag[A]) extends Matrix[A] { lhs =>
    def apply(x: Int, y: Int): A = arr(y * dim.n + x)

    def update(x: Int, y: Int, a: A): Unit = arr(y * dim.n + x) = a

    def map[B: Rig: ClassTag](f: A => B): Matrix[B] =
      ArrayMatrix(arr.map(f))

    def +(rhs: Matrix[A]): Matrix[A] =
      Matrix((x, y) => lhs(x, y) + rhs(x, y))

    def *(rhs: Matrix[A]): Matrix[A] =
      Matrix { (x, y) =>
        var total = Rig[A].zero
        cfor(0)(_ < dim.n, _ + 1)(j => total += lhs(j, y) * rhs(x, j))
        total
      }
  }

  import KleeneExample._

  // a Graph is just an adjacency matrix
  type Graph = Matrix[Boolean]

  object Graph {
    /**
     * Build a graph from a list of tuples (edges). Each tuple (a, b)
     * represents an edge from a to b.
     */
    def apply(edges: (Int, Int)*)(implicit dim: Dim): Graph = {
      val m = ArrayMatrix(Array.fill[Boolean](dim.n * dim.n)(false))
      edges.foreach { case (from, to) => m(to, from) = true }
      m
    }
  }

  /**
   * Pretty print a Graph.
   */
  def printMatrix(g: Graph) {
    val n = g.dim.n
    cfor(0)(_ < n, _ + 1) { y =>
      cfor(0)(_ < n, _ + 1) { x =>
        print(if (g(x, y)) 'x' else '.')
        print(' ')
      }
      print('\n')
    }
  }
}

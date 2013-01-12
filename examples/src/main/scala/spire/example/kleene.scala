package spire.example

import Predef.{any2stringadd => _, intWrapper => _, _}

import spire.algebra._
import spire.implicits._
import spire.math._
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

/**
 * A KleeneAlgebra is a Rig with asteration (kstar).
 *
 * Laws:
 * 1. a + a = a
 * 2. a * x + x = x  ==> a.kstar * x + x = x
 * 3. x * a + x = x  ==>  x * a.kstar + x = x
 */
trait KleeneAlgebra[A] extends Rig[A] {
  // one of these must be overridden in any type class instance
  def kstar(a: A): A = plus(one, kplus(a))
  def kplus(a: A): A = times(a, kstar(a))
}


object KleeneAlgebra {
  def apply[A](implicit ev: KleeneAlgebra[A]) = ev

  implicit object BooleanHasKleeneAlgebra extends KleeneAlgebra[Boolean] with BooleanIsRig {
    override def kstar(x: Boolean) = true
  }

  implicit def matrixHasKleeneAlgebra[A](implicit dim: Dim, ka: KleeneAlgebra[A], ct: ClassTag[A]) =
    new KleeneAlgebra[Matrix[A]] {
      def zero: Matrix[A] = Matrix.zero
      def one: Matrix[A] = Matrix.one
      def plus(x: Matrix[A], y: Matrix[A]) = x + y
      def times(x: Matrix[A], y: Matrix[A]) = x * y

      override def kplus(m: Matrix[A]) = {
        def f(k: Int, m: Matrix[A]) = Matrix[A] { (x, y) =>
          m(x, y) + m(k, y) * ka.kstar(m(k, k)) * m(x, k)
        }
        @tailrec def loop(m: Matrix[A], i: Int): Matrix[A] =
          if (i >= 0) loop(f(i, m), i - 1) else m
        loop(m, dim.n - 1)
      }
    }

  implicit def ExprHasKleeneAlgebra[A] = new KleeneAlgebra[Expr[A]] {
    def zero: Expr[A] = Nul()
    def one: Expr[A] = Empty()
    def plus(x: Expr[A], y: Expr[A]): Expr[A] = (x, y) match {
      case (Nul(), e) => e
      case (e, Nul()) => e
      case (Empty(), Empty()) => Empty()
      case (Empty(), Star(e)) => Star(e)
      case (Star(e), Empty()) => Star(e)
      case (e1, e2) => Or(e1, e2)
    }
    def times(x: Expr[A], y: Expr[A]): Expr[A] = (x, y) match {
      case (Nul(), _) => Nul()
      case (_, Nul()) => Nul()
      case (Empty(), e) => e
      case (e, Empty()) => e
      case (e1, e2) => Then(e1, e2)
    }
    override def kstar(x: Expr[A]): Expr[A] = x match {
      case Nul() => Empty()
      case Empty() => Empty()
      case Star(e) => kstar(e)
      case _ => Star(x)
    }
  }

  implicit def TropicalHasKleeneAlgebra[A: Order: Rig] = new KleeneAlgebra[Tropical[A]] {
    def zero: Tropical[A] = Infinity()
    def one: Tropical[A] = Tropical(Rig[A].zero)
    def plus(x: Tropical[A], y: Tropical[A]): Tropical[A] = (x, y) match {
      case (Infinity(), t) => t
      case (t, Infinity()) => t
      case (Finite(a1), Finite(a2)) => Tropical(a1 min a2)
    }
    def times(x: Tropical[A], y: Tropical[A]): Tropical[A] = (x, y) match {
      case (Infinity(), _) => Infinity()
      case (_, Infinity()) => Infinity()
      case (Finite(a1), Finite(a2)) => Tropical(a1 + a2)
    }
    override def kstar(x: Tropical[A]): Tropical[A] = one
  }
}

sealed trait Expr[A]
case class Var[A](a: A) extends Expr[A]
case class Or[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
case class Then[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
case class Star[A](lhs: Expr[A]) extends Expr[A]
case class Empty[A]() extends Expr[A]
case class Nul[A]() extends Expr[A]

sealed trait Tropical[A]
case class Finite[A](a: A) extends Tropical[A]
case class Infinity[A]() extends Tropical[A]

object Tropical {
  def apply[A](a: A): Tropical[A] = Finite(a)
}

object KleeneExample {
  // none of this should be necessary! arghghh!!!!
  implicit val ihs = Show.IntHasShow
  implicit val bhs = Show.BooleanHasShow
  implicit val ehs = Show.EdgeHasShow
  implicit def ohs[A: Show] = Show.optionHasShow[A]
  implicit def exprhs[A: Show] = Show.exprHasShow[A]
  implicit def ths[A: Show] = Show.tropicalHasShow[A]

  def graphExample() {
    // our example graph will be 5x5
    implicit val dim = Dim(5)

    // build our example graph from a sequence of directional edges
    val example: Matrix[Boolean] = Graph(
      Edge(0, 1), Edge(1, 2), Edge(2, 3), Edge(2, 4), Edge(3, 1), Edge(4, 3)
    )

    // examine the graph
    val kb = KleeneAlgebra[Matrix[Boolean]]
    println("adjacency:\n%s" format example.show)
    println("reflexive-transitive closure:\n%s" format kb.kstar(example).show)
    println("transitive closure:\n%s" format kb.kplus(example).show)

    val labeled = LabeledGraph(example)
    println("labels:\n%s" format labeled.show)

    val expred = ExprGraph(labeled)
    val ke = KleeneAlgebra[Matrix[Expr[Edge]]]
    println("expr:\n%s" format expred.show)
    println("re:\n%s" format ke.kstar(expred).show)
  }

  def pathExample() {
    // our example graph will be 5x5
    implicit val dim = Dim(6)

    val edges: List[(Edge, Int)] = List(
      (Edge(0, 1), 7), (Edge(0, 2), 9), (Edge(0, 5), 14),
      (Edge(1, 2), 10), (Edge(1, 3), 15),
      (Edge(2, 3), 11), (Edge(2, 5), 2),
      (Edge(3, 4), 6),
      (Edge(4, 5), 9)
    )
    val kt = KleeneAlgebra[Matrix[Tropical[Int]]]
    val weighted: Matrix[Tropical[Int]] = {
      val m = ArrayMatrix(Array.fill[Tropical[Int]](dim.n * dim.n)(Infinity()))
      edges.foreach {
        case (Edge(y, x), n) =>
          // undirected graph
          m(x, y) = Tropical(n)
          m(y, x) = Tropical(n)
      }
      m
    }
    println("weights:\n%s" format weighted.show)
    println("shortest:\n%s" format kt.kstar(weighted).show)
  }

  def main(args: Array[String]) {
    graphExample()
    pathExample()
  }
}

/**
 * Build a graph from a list of tuples (edges). Each tuple (a, b)
 * represents an edge from a to b.
 *
 * A graph is just an adjacency matrix, i.e. Matrix[Boolean].
 */
object Graph {
  def apply(edges: Edge*)(implicit dim: Dim): Matrix[Boolean] = {
    val m = ArrayMatrix(Array.fill[Boolean](dim.n * dim.n)(false))
    edges.foreach { case Edge(from, to) => m(to, from) = true }
    m
  }
}

/**
 *
 */
object LabeledGraph {
  def apply(m: Matrix[Boolean])(implicit dim: Dim) = Matrix[Option[Edge]] { (x, y) =>
    if (m(x, y)) Some(Edge(y, x)) else None
  }
}

/**
 *
 */
object ExprGraph {
  def apply(m: Matrix[Option[Edge]])(implicit dim: Dim) = Matrix[Expr[Edge]] { (x, y) =>
    m(x, y).map(Var.apply).getOrElse(Nul())
  }
}

/**
 * Naive matrix trait.
 */
trait Matrix[A] { lhs =>
  def dim: Dim
  def apply(x: Int, y: Int): A
  def map[B: Rig: ClassTag](f: A => B): Matrix[B]
  def +(rhs: Matrix[A])(implicit rig: Rig[A]): Matrix[A]
  def *(rhs: Matrix[A])(implicit rig: Rig[A]): Matrix[A]

  def show(implicit ev: Show[A]): String = {
    val s = Show[A]
    val n = dim.n
    val lines = Array.fill(n)("")
    cfor(0)(_ < n, _ + 1) { x =>
      cfor(0)(_ < n, _ + 1)(y => lines(y) += s.show(apply(x, y)) + " ")
      val len = lines.foldLeft(0)(_ max _.length)
      cfor(0)(_ < n, _ + 1)(y => lines(y) += " " * (len - lines(y).length))
    }
    lines.mkString("\n") + "\n"
  }
}

object Matrix {
  /**
   * Builds a Matrix[A] given a function (Int, Int) => A and an implicit Dim
   * to provide the dimensions over which to run the function.
   */
  def apply[A: ClassTag](f: (Int, Int) => A)(implicit dim: Dim): Matrix[A] = {
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
case class ArrayMatrix[A](arr: Array[A])(implicit val dim: Dim, ct: ClassTag[A]) extends Matrix[A] { lhs =>
  def apply(x: Int, y: Int): A = arr(y * dim.n + x)

  def update(x: Int, y: Int, a: A): Unit = arr(y * dim.n + x) = a

  def map[B: Rig: ClassTag](f: A => B): Matrix[B] =
    ArrayMatrix(arr.map(f))

  def +(rhs: Matrix[A])(implicit rig: Rig[A]): Matrix[A] =
    Matrix((x, y) => lhs(x, y) + rhs(x, y))

  def *(rhs: Matrix[A])(implicit rig: Rig[A]): Matrix[A] =
    Matrix { (x, y) =>
      var total = rig.zero
      cfor(0)(_ < dim.n, _ + 1)(j => total += lhs(j, y) * rhs(x, j))
      total
    }
}

/**
 * Show is a type class we'll use to control how types should display.
 */
trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A](implicit ev: Show[A]) = ev

  trait UseToString[A] extends Show[A] {
    def show(a: A) = a.toString
  }

  implicit object IntHasShow extends UseToString[Int]

  implicit object BooleanHasShow extends Show[Boolean] {
    def show(a: Boolean) = if (a) "x" else "."
  }

  implicit object EdgeHasShow extends Show[Edge] {
    def show(e: Edge) = "(%c%c)" format ('A' + e.from, 'A' + e.to)
  }

  implicit def optionHasShow[A](implicit ev: Show[A]) = new Show[Option[A]] {
    def show(a: Option[A]) = a.map(ev.show).toString
  }

  implicit def tropicalHasShow[A](implicit ev: Show[A]) = new Show[Tropical[A]] {
    def show(t: Tropical[A]) = t match {
      case Infinity() => "∞"
      case Finite(a) => ev.show(a)
    }
  }

  implicit def exprHasShow[A](implicit ev: Show[A]) = new Show[Expr[A]] {
    def show(e: Expr[A]) = e match {
      case Var(a) => ev.show(a)
      case Empty() => "ε"
      case Nul() => "∅"
      case Star(x) => "(" + show(x) + ")*"
      case Or(x, y) => "(" + show(x) + "|" + show(y) + ")"
      case Then(x, y) => show(x) + show(y)
    }
  }
}

case class Edge(from: Int, to: Int)

/**
 * Dim is a cute little class that let's us have implicit size information.
 * 
 * This is to work around the fact that we don't currently have
 * implementations of Bounded[A] or Ix[A] like Haskell does.
 *
 * Dim is probably not robust enough for real world use.
 */
case class Dim(n: Int)

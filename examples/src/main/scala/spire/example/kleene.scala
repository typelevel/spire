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

object Xyz {

  /**
   * Show is a type class we'll use to control how types should display.
   */
  trait Show[A] {
    def show(a: A): String
  }
  object Show {
    def apply[A](implicit ev: Show[A]) = ev
  }
  implicit class ShowOps[A: Show](a: A) {
    def show: String = Show[A].show(a)
  }

  // Show[A] instances for built-in types
  implicit object IntHasShow extends Show[Int] {
    def show(a: Int) = a.toString
  }
  implicit object BooleanHasShow extends Show[Boolean] {
    def show(a: Boolean) = if (a) "x" else "."
  }
  implicit def optionHasShow[A](implicit ev: Show[A]) = new Show[Option[A]] {
    def show(a: Option[A]) = a.map(ev.show).getOrElse("-")
  }
  implicit def listHasShow[A](implicit ev: Show[A]) = new Show[List[A]] {
    def show(a: List[A]) = a.map(ev.show).mkString("[", ",", "]")
  }
  implicit def streamHasShow[A](implicit ev: Show[A]) = new Show[Stream[A]] {
    def show(s: Stream[A]) =
      if (s.isEmpty) "[]" else "[%s,...]" format ev.show(s.head)
  }

  /**
   * A Kleene is a Rig with asteration (kstar).
   *
   * Laws:
   * 1. a + a = a
   * 2. a * x + x = x  ==> a.kstar * x + x = x
   * 3. x * a + x = x  ==>  x * a.kstar + x = x
   */
  trait Kleene[A] extends Rig[A] {
    // one of these must be overridden in any type class instance
    def kstar(a: A): A = plus(one, kplus(a))
    def kplus(a: A): A = times(a, kstar(a))
  }
  object Kleene {
    def apply[A](implicit ev: Kleene[A]) = ev
  }
  implicit def kleeneHasRig[A: Kleene]: Rig[A] = Kleene[A]
  implicit class KleeneOps[A: Kleene](a: A) {
    def kstar: A = Kleene[A].kstar(a)
    def kplus: A = Kleene[A].kplus(a)
  }

  // Kleene[A] instances for built-in types
  implicit object BooleanHasKleene extends Kleene[Boolean] with BooleanIsRig {
    override def kstar(x: Boolean) = true
  }

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
    def map[B: ClassTag](f: A => B): Matrix[B]
    def +(rhs: Matrix[A])(implicit rig: Rig[A]): Matrix[A]
    def *(rhs: Matrix[A])(implicit rig: Rig[A]): Matrix[A]
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

    def map[B: ClassTag](f: A => B): Matrix[B] =
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

  // type class instance for Show[Matrix[A]]
  implicit def matrixHasShow[A](implicit ev: Show[A]) = new Show[Matrix[A]] {
    def show(m: Matrix[A]): String = {
      val s = Show[A]
      val n = m.dim.n
      val lines = Array.fill(n)("")
      cfor(0)(_ < n, _ + 1) { x =>
        cfor(0)(_ < n, _ + 1)(y => lines(y) += s.show(m(x, y)) + " ")
        val len = lines.foldLeft(0)(_ max _.length)
        cfor(0)(_ < n, _ + 1)(y => lines(y) += " " * (len - lines(y).length))
      }
      lines.mkString("\n") + "\n"
    }
  }

  // type class instance for Kleene[Matrix[A]]
  implicit def matrixHasKleene[A](implicit dim: Dim, ka: Kleene[A], ct: ClassTag[A]) =
    new Kleene[Matrix[A]] {
      def zero: Matrix[A] = Matrix.zero
      def one: Matrix[A] = Matrix.one
      def plus(x: Matrix[A], y: Matrix[A]) = x + y
      def times(x: Matrix[A], y: Matrix[A]) = x * y

      override def kplus(m: Matrix[A]) = {
        def f(k: Int, m: Matrix[A]) = Matrix[A] { (x, y) =>
          m(x, y) + m(k, y) * m(k, k).kstar * m(x, k)
        }
        @tailrec def loop(m: Matrix[A], i: Int): Matrix[A] =
          if (i >= 0) loop(f(i, m), i - 1) else m
        loop(m, dim.n - 1)
      }
    }

  /**
   * Edge is a simple class used to construct adjacency matrices.
   * 
   * It's important to remember that edges go: y -> x.
   * 
   * Thus from is the y-coordinate and to is the x-coordinate.
   */
  case class Edge(from: Int, to: Int)

  // type class instance for Show[Edge]
  implicit object EdgeHasShow extends Show[Edge] {
    def show(e: Edge) = "(%c%c)" format ('A' + e.from, 'A' + e.to)
  }


  /**
   * Graph provides functions for constructing an adjacency matrices.
   */
  object Graph {
    def apply(edges: Edge*)(implicit dim: Dim): Matrix[Boolean] = {
      val m = ArrayMatrix(Array.fill[Boolean](dim.n * dim.n)(false))
      edges.foreach { case Edge(from, to) => m(to, from) = true }
      m
    }
  }
  object LabeledGraph {
    def apply(m: Matrix[Boolean])(implicit dim: Dim) = Matrix[Option[Edge]] { (x, y) =>
      if (m(x, y)) Some(Edge(y, x)) else None
    }
  }


  /**
   * Expr[A] implements an AST for regular expressions.
   * 
   * Basic regular consist of the following:
   *  1. the empty set (Nul)        -- a set with no strings
   *  2. the empty string (Empty)   -- set containing the empty string
   *  3. literal strings (Var(a))   -- set containing a
   *  4. concatenation (Then(a, b)) -- set of all xy, for x in a, y in b
   *  5. alternation (Or(a, b))     -- union set of a and b
   *  6. kleene star (Star(a))      -- set produced by 0+ concatenations from a
   * 
   * For example, (a|bc)* includes "", "a", "bc", "abcaaaabc" but not "bc".
   */
  sealed trait Expr[A]
  case class Var[A](a: A) extends Expr[A]
  case class Or[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
  case class Then[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
  case class Star[A](lhs: Expr[A]) extends Expr[A]
  case class Empty[A]() extends Expr[A]
  case class Nul[A]() extends Expr[A]
  object Expr {
    def apply[A](a: A): Expr[A] = Var(a)
  }

  // type class instance for Show[Expr[A]]
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

  // type class instance for Kleene[Expr[A]]
  implicit def exprHasKleene[A] = new Kleene[Expr[A]] {
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


  /**
   * Tropical represents a finite quantity between zero and infinity.
   */
  sealed trait Tropical[A]
  case class Finite[A](a: A) extends Tropical[A]
  case class Infinity[A]() extends Tropical[A]

  object Tropical {
    def apply[A](a: A): Tropical[A] = Finite(a)
  }

  implicit def tropicalHasShow[A: Show] = new Show[Tropical[A]] {
    def show(t: Tropical[A]) = t match {
      case Finite(a) => Show[A].show(a)
      case Infinity() => "∞"
    }
  }

  implicit def tropicalHasOrder[A](implicit ord: Order[A]) = new Order[Tropical[A]] {
    def eqv(x: Tropical[A], y: Tropical[A]) = compare(x, y) == 0
    def compare(x: Tropical[A], y: Tropical[A]) = (x, y) match {
      case (Infinity(), Infinity()) => 0
      case (Infinity(), _) => 1
      case (_, Infinity()) => -1
      case (Finite(a1), Finite(a2)) => ord.compare(a1, a2)
    }
  }

  implicit def TropicalHasKleene[A: Order: Rig] = new Kleene[Tropical[A]] {
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


  /**
   * ShortestPath is a data structure which will track two things:
   *  1. the path's cost, as Tropical[A]
   *  2. the path itself, as B
   * Any impossible path will have Infinity() as its cost.
   */
  case class ShortestPath[A, B](a: Tropical[A], b: B) {
    def map[C](f: B => C) = ShortestPath[A, C](a, f(b))
  }

  // type class instance for Show[ShortestPath[A, B]]
  implicit def spHasShow[A: Show, B: Show] = new Show[ShortestPath[A, B]] {
    def show(p: ShortestPath[A, B]) = "%s[%s]" format (p.b.show, p.a.show)
  }

  // type class instance for Kleene[ShortestPath[A, B]]
  implicit def shortestPathHasKleene[A, B](implicit rig: Rig[Tropical[A]], ord: Order[Tropical[A]], kb: Kleene[B]) =
    new Kleene[ShortestPath[A, B]] {
      def zero = ShortestPath(rig.zero, kb.zero)

      def one = ShortestPath(rig.one, kb.one)

      def plus(x: ShortestPath[A, B], y: ShortestPath[A, B]) = (x.a compare y.a) match {
        case -1 => x
        case 0 => ShortestPath(x.a + y.a, x.b + y.b)
        case 1 => y
      }

      def times(x: ShortestPath[A, B], y: ShortestPath[A, B]) =
        ShortestPath(x.a * y.a, x.b * y.b)

      override def kstar(x: ShortestPath[A, B]) =
        ShortestPath(rig.one, if (x.a === rig.one) x.b.kstar else kb.one)
    }


  /**
   * Language represents the set of every valid string in a regular
   * language. Each W is a valid character, each Stream[W] is a (lazy)
   * string, and SS[W] (e.g. Stream[Stream[W]]) is the complete set of
   * all strings.
   */
  case class Language[W](wss: SS[W]) {
    def someWord: Option[List[W]] = wss.headOption.map(_.toList)
  }

  // handy type alias
  type SS[W] = Stream[Stream[W]]

  // type class instance for Show[Language[W]]
  implicit def languageHasShow[W: Show] = new Show[Language[W]] {
    def show(l: Language[W]) = Show[SS[W]].show(l.wss)
  }

  // type class instance for Kleene[Language[W]]
  implicit def languageHasKleene[W] = new Kleene[Language[W]] {
    def zero: Language[W] = Language(Stream.empty[Stream[W]])
    def one: Language[W] = Language(Stream(Stream.empty[W]))

    def plus(x: Language[W], y: Language[W]): Language[W] = {
      def interleave(ws1: SS[W], ws2: SS[W]): SS[W] =
        if (ws1.isEmpty) ws2 else ws1.head #:: interleave(ws2, ws1)
      Language(interleave(x.wss, y.wss))
    }

    def times(x: Language[W], y: Language[W]): Language[W] =
      Language(x.wss.flatMap(ws1 => y.wss.map(ws2 => ws1 #::: ws2)))

    override def kstar(x: Language[W]): Language[W] = {
      def plusList(z: Language[W]): Language[W] =
        if (z.wss.isEmpty) zero else Language(z.wss #::: kstar(z).wss)
      plus(one, plusList(x))
    }
  }


  def graphExample() {
    // our example graph will be 5x5
    implicit val dim = Dim(5)

    // build our example graph from a sequence of directional edges
    val edges: List[Edge] = List(
      Edge(0, 1), Edge(1, 2), Edge(2, 3), Edge(2, 4), Edge(3, 1), Edge(4, 3)
    )

    val example: Matrix[Boolean] = Graph(edges:_*)

    // examine the graph
    println("adjacency matrix:\n%s" format example.show)
    println("reflexive-transitive closure:\n%s" format example.kstar.show)
    println("transitive closure:\n%s" format example.kplus.show)

    val labeled = LabeledGraph(example)
    println("labels:\n%s" format labeled.show)

    val expred = labeled.map(_.map(Expr.apply).getOrElse(Nul()))

    println("expr:\n%s" format expred.show)
    println("re:\n%s" format expred.kstar.show)
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
    println("least-cost:\n%s" format weighted.kstar.show)

    val annotated = Matrix[ShortestPath[Int, Expr[Edge]]] { (x, y) =>
      weighted(x, y) match {
        case Infinity() =>
          ShortestPath(Infinity(), Kleene[Expr[Edge]].zero)
        case Finite(n) =>
          ShortestPath(Finite(n), Var(Edge(y, x)))
      }
    }

    println("annotated-re:\n" + annotated.show)
    println("shortest-path-re:\n" + annotated.kstar.show)

    val langed = Matrix[ShortestPath[Int, Language[Edge]]] { (x, y) =>
      weighted(x, y) match {
        case Infinity() =>
          ShortestPath(Infinity(), Kleene[Language[Edge]].zero)
        case Finite(n) =>
          ShortestPath(Finite(n), Language(Stream(Stream(Edge(y, x)))))
      }
    }

    println("l-annotated:\n" + langed.show)
    println("l-shortest-path:\n" + langed.kstar.map(_.b.someWord).show)

    def evalExpr[A, B](expr: Expr[A])(f: A => B)(implicit k: Kleene[B]): B =
      expr match {
        case Nul() => k.zero
        case Empty() => k.one
        case Var(a) => f(a)
        case Star(x) => evalExpr(x)(f).kstar
        case Or(x, y) => evalExpr(x)(f) + evalExpr(y)(f)
        case Then(x, y) => evalExpr(x)(f) * evalExpr(y)(f)
      }
  }

  def main(args: Array[String]) {
    graphExample()
    pathExample()
  }


}

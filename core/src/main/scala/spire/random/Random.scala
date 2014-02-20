package spire.random

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax.cfor._

import scala.collection.mutable.ArrayBuffer
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

sealed trait Op[+A] {

  def flatMap[B](f: A => Op[B]): Op[B] =
    this match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x).flatMap(f))
      case o => FlatMap(o, f)
    }

  def map[B](f: A => B): Op[B] =
    flatMap(a => Const(f(a)))

  def resume(gen: Generator): Either[() => Op[A], A] =
    this match {
      case Const(a) =>
        Right(a)
      case More(k) =>
        Left(k)
      case Next(f) =>
        Right(f(gen))
      case FlatMap(a, f) =>
        a match {
          case Const(x) => f(x).resume(gen)
          case More(k) => Left(() => FlatMap(k(), f))
          case Next(g) => f(g(gen)).resume(gen)
          case FlatMap(b, g) => FlatMap(b, (x: Any) => FlatMap(g(x), f)).resume(gen)
        }
    }

  def run(gen: Generator): A = {
    def loop(e: Either[() => Op[A], A]): A = e match {
      case Right(a) => a
      case Left(k) => loop(k().resume(gen))
    }
    loop(resume(gen))
  }
}

case class Const[+A](a: A) extends Op[A]
case class More[+A](k: () => Op[A]) extends Op[A]
case class Next[+A](f: Generator => A) extends Op[A]
case class FlatMap[A, +B](sub: Op[A], k: A => Op[B]) extends Op[B]

object Random extends RandomCompanion[rng.Cmwc5] {
  def initGenerator() = rng.Cmwc5.fromTime()

  def spawn[B](op: Op[B]) = new RandomCmwc5(op)
}

trait RandomCompanion[G <: Generator] {
  type R[X] = Random[X, G]

  def initGenerator(): G //IO

  def generatorFromSeed(seed: Seed): G = {
    val gen = initGenerator()
    gen.setSeedBytes(seed.bytes)
    gen
  }

  def spawn[B](op: Op[B]): R[B]

  def next[B](f: Generator => B): R[B] = spawn(Next(f))

  def constant[B](b: B) = spawn(Const(b))

  def unit = constant(Unit)
  def boolean = next(_.nextBoolean)
  def byte = next(_.nextInt.toByte)
  def short = next(_.nextInt.toShort)
  def char = next(_.nextInt.toChar)

  def int = next(_.nextInt)
  def int(n: Int) = next(_.nextInt(n))
  def int(n1: Int, n2: Int) = next(_.nextInt(n1, n2))

  def float = next(_.nextFloat)
  def long = next(_.nextLong)
  def double = next(_.nextDouble)

  def string(size: Size): R[String] =
    size.assemble(stringOfSize)(int(_, _))

  def stringOfSize(n: Int): R[String] =
    char.listOfSize(n).map(_.mkString)

  implicit class RandomOps[A](lhs: R[A]) {

    def collection[CC[_]](size: Size)(implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): Random[CC[A], G] =
      size.assemble(collectionOfSize(_))(int(_, _))

    def collectionOfSize[CC[_]](n: Int)(implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): Random[CC[A], G] =
      lhs.listOfSize(n).map { as =>
        val b = cbf()
        as.foreach { b += _ }
        b.result
      }
  }

  def tuple2[A, B](r1: R[A], r2: R[B]): R[(A, B)] =
    r1 and r2
  def tuple3[A, B, C](r1: R[A], r2: R[B], r3: R[C]): R[(A, B, C)] =
    for { a <- r1; b <- r2; c <- r3 } yield (a, b, c)
  def tuple4[A, B, C, D](r1: R[A], r2: R[B], r3: R[C], r4: R[D]): R[(A, B, C, D)] =
    for { a <- r1; b <- r2; c <- r3; d <- r4 } yield (a, b, c, d)
}

abstract class Random[+A, G <: Generator](val op: Op[A]) { self =>

  def companion: RandomCompanion[G]

  def map[B](f: A => B): Random[B, G] =
    companion.spawn(op.map(f))

  def flatMap[B](f: A => Random[B, G]): Random[B, G] =
    companion.spawn(op.flatMap(f(_).op))

  def run(): A =
    op.run(companion.initGenerator) //IO
  
  def run(seed: Seed): A = { //IO
    val gen = companion.initGenerator()
    gen.setSeedBytes(seed.bytes)
    op.run(gen)
  }

  def some: Random[Some[A], G] = map(Some(_))
  def left: Random[Left[A, Nothing], G] = map(Left(_))
  def right: Random[Right[Nothing, A], G] = map(Right(_))

  def option: Random[Option[A], G] =
    companion.boolean.flatMap(b => if (b) some else companion.constant(None))

  def or[B](that: Random[B, G]): Random[Either[A, B], G] =
    companion.boolean.flatMap(b => if (b) left else that.right)

  def and[B](that: Random[B, G]): Random[(A, B), G] =
    for { a <- this; b <- that } yield (a, b)

  def recurse[B](body: => Random[B, G]): Random[B, G] =
    companion.spawn(More(() => body.op))

  def listOfSize(n: Int): Random[List[A], G] = {
    def loop(n: Int): Op[List[A]] =
      if (n <= 0) Const(Nil) else for {
        as <- More(() => loop(n - 1))
        a <- More(() => op)
      } yield a :: as
    companion.spawn(loop(n))
  }

  def list(size: Size): Random[List[A], G] =
    size.assemble(listOfSize)(companion.int(_, _))
}

class RandomCmwc5[+A](op: Op[A]) extends Random[A, rng.Cmwc5](op) {
  def companion = Random
}

sealed trait Size {
  def assemble[A, G <: Generator](f: Int => Random[A, G])(g: (Int, Int) => Random[Int, G]): Random[A, G] =
    this match {
      case Exact(n) => f(n)
      case Between(n1, n2) => g(n1, n2).flatMap(f)
    }
}

case class Exact(n: Int) extends Size
case class Between(n1: Int, n2: Int) extends Size

object Size {
  def apply(n: Int): Size = Exact(n)
  def upTo(n: Int): Size = Between(0, n)
  def between(n1: Int, n2: Int): Size = Between(n1, n2)
}

class Seed private[spire] (private[spire] val bytes: Array[Byte])

object Seed {
  val zero = Seed(Array[Byte](0, 0, 0, 0))
  def apply(n: Int): Seed = new Seed(spire.util.Pack.intToBytes(n))
  def apply(n: Long): Seed = new Seed(spire.util.Pack.longToBytes(n))
  def apply(bytes: Array[Byte]): Seed = new Seed(bytes.clone)
}

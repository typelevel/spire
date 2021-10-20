/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package random

import scala.collection.Factory

sealed trait Op[+A] {

  def flatMap[B](f: A => Op[B]): Op[B] =
    this match {
      case fm: FlatMap[a, _] => FlatMap(fm.sub, (x: a) => fm.k(x).flatMap(f))
      case o                 => FlatMap(o, f)
    }

  def map[B](f: A => B): Op[B] =
    flatMap(a => Const(f(a)))

  @tailrec
  final def resume(gen: Generator): Either[() => Op[A], A] =
    this match {
      case Const(a) =>
        Right(a)
      case More(k) =>
        Left(k)
      case Next(f) =>
        Right(f(gen))
      case FlatMap(a, f) =>
        a match {
          case Const(x)          => f(x).resume(gen)
          case More(k)           => Left(() => FlatMap(k(), f))
          case Next(g)           => f(g(gen)).resume(gen)
          case fm: FlatMap[b, _] => (FlatMap(fm.sub, (x: b) => fm.k(x).flatMap(f)): Op[A]).resume(gen)
        }
    }

  def run(gen: Generator): A = {
    def loop(e: Either[() => Op[A], A]): A = e match {
      case Right(a) => a
      case Left(k)  => loop(k().resume(gen))
    }
    loop(resume(gen))
  }
}

case class Const[+A](a: A) extends Op[A]
case class More[+A](k: () => Op[A]) extends Op[A]
case class Next[+A](f: Generator => A) extends Op[A]
case class FlatMap[A, +B](sub: Op[A], k: A => Op[B]) extends Op[B]

object Random extends RandomCompanion[rng.Cmwc5] {
  def initGenerator: spire.random.rng.Cmwc5 = rng.Cmwc5.fromTime()

  def spawn[B](op: Op[B]): RandomCmwc5[B] = new RandomCmwc5(op)
}

trait RandomCompanion[G <: Generator] { self =>
  type R[X] = Random[X, G]

  def initGenerator: G //IO

  def generatorFromSeed(seed: Seed): G = {
    val gen = initGenerator
    gen.setSeedBytes(seed.bytes)
    gen
  }

  def spawn[B](op: Op[B]): R[B]

  def next[B](f: Generator => B): R[B] = spawn(Next(f))

  def fromDist[B](dist: Dist[B]): R[B] = spawn(Next(g => dist(g)))

  def constant[B](b: B): R[B] = spawn(Const(b))

  def unit: R[Unit] = constant(())
  def boolean: R[Boolean] = next(_.nextBoolean())
  def byte: R[Byte] = next(_.nextInt().toByte)
  def short: R[Short] = next(_.nextInt().toShort)
  def char: R[Char] = next(_.nextInt().toChar)

  def int: R[Int] = next(_.nextInt())
  def int(n: Int): R[Int] = next(_.nextInt(n))
  def int(n1: Int, n2: Int): R[Int] = next(_.nextInt(n1, n2))

  def float: R[Float] = next(_.nextFloat())
  def long: R[Long] = next(_.nextLong())
  def double: R[Double] = next(_.nextDouble())

  def string(size: Size): R[String] =
    size.random(this).flatMap(stringOfSize)

  def stringOfSize(n: Int): Random[String, G] =
    char.foldLeftOfSize(n)(new StringBuilder) { (sb, c) => sb.append(c); sb }.map(_.toString)

  implicit class RandomOps[A](lhs: R[A]) {
    def collection[CC[_]](size: Size)(implicit cbf: Factory[A, CC[A]]): Random[CC[A], G] =
      size.random(self).flatMap(collectionOfSize(_))

    def collectionOfSize[CC[_]](n: Int)(implicit cbf: Factory[A, CC[A]]): Random[CC[A], G] =
      foldLeftOfSize(n)(cbf.newBuilder) { (b, a) => b += a; b }.map(_.result())

    def foldLeftOfSize[B](n: Int)(init: => B)(f: (B, A) => B): Random[B, G] = {
      def loop(n: Int, ma: Op[A]): Op[B] =
        if (n <= 0) Const(init)
        else More(() => loop(n - 1, ma)).flatMap(b => ma.map(a => f(b, a)))
      spawn(loop(n, More(() => lhs.op)))
    }

    def unfold[B](init: B)(f: (B, A) => Option[B]): Random[B, G] = {
      def loop(mb: Op[B], ma: Op[A]): Op[B] =
        mb.flatMap(b =>
          ma.flatMap(a =>
            f(b, a) match {
              case Some(b2) => More(() => loop(Const(b2), ma))
              case None     => Const(b)
            }
          )
        )
      spawn(loop(Const(init), More(() => lhs.op)))
    }
  }

  def tuple2[A, B](r1: R[A], r2: R[B]): R[(A, B)] =
    r1.and(r2)
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

  def run: A =
    op.run(companion.initGenerator) //IO

  def run(seed: Seed): A = { //IO
    val gen = companion.initGenerator
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

  def list(size: Size): Random[List[A], G] =
    size.random(companion).flatMap(listOfSize)

  def listOfSize(n: Int): Random[List[A], G] =
    companion.RandomOps(this).foldLeftOfSize(n)(List.empty[A])((as, a) => a :: as)
}

class RandomCmwc5[+A](op: Op[A]) extends Random[A, rng.Cmwc5](op) {
  def companion: Random.type = Random
}

sealed trait Size {
  def random[G <: Generator](r: RandomCompanion[G]): Random[Int, G]
}

object Size {
  def apply(n: Int): Size = Exact(n)
  def upTo(n: Int): Size = Between(0, n)
  def between(n1: Int, n2: Int): Size = Between(n1, n2)

  case class Exact(n: Int) extends Size {
    def random[G <: Generator](r: RandomCompanion[G]): Random[Int, G] = r.spawn(Const(n))
  }

  case class Between(n1: Int, n2: Int) extends Size {
    def random[G <: Generator](r: RandomCompanion[G]): Random[Int, G] = r.int(n1, n2)
  }
}

class Seed private[spire] (private[spire] val bytes: Array[Byte])

object Seed {
  val zero = Seed(Array[Byte](0, 0, 0, 0))
  def apply(n: Int): Seed = new Seed(spire.util.Pack.intToBytes(n))
  def apply(n: Long): Seed = new Seed(spire.util.Pack.longToBytes(n))
  def apply(bytes: Array[Byte]): Seed = new Seed(bytes.clone)
}

package spire
package random

import scala.collection.Factory

sealed trait Op[+A] {

  def flatMap[B](f: A => Op[B]): Op[B] =
    this match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x).flatMap(f))
      case o             => FlatMap(o, f)
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
          case Const(x)      => f(x).resume(gen)
          case More(k)       => Left(() => FlatMap(k(), f))
          case Next(g)       => f(g(gen)).resume(gen)
          case FlatMap(b, g) => (FlatMap(b, (x: Any) => FlatMap(g(x), f)): Op[A]).resume(gen)
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


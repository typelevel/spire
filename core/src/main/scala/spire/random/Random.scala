package spire.random

import spire.algebra._
import spire.implicits._
import spire.math._

import scala.collection.mutable.ArrayBuffer
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.reflect.ClassTag

object Random extends RandomCompanion[gen.Cmwc5] {
  def initGenerator() = gen.Cmwc5.fromTime()
  def spawn[B](d: Dist[B]) = RandomCmwc5(d)
}

trait RandomCompanion[G <: Generator] {
  def initGenerator(): G //IO

  def generatorFromSeed(seed: Seed): G = {
    val gen = initGenerator()
    gen.setSeedBytes(seed.bytes)
    gen
  }

  def spawn[B](d: Dist[B]): Random[B, G]

  def constant[B](b: B) = spawn(Dist.constant(b))

  def int(n: Int) = spawn(Dist.gen(_.nextInt(n)))

  def boolean = spawn(Dist.boolean)
  def byte = spawn(Dist.byte)
  def short = spawn(Dist.short)
  def char = spawn(Dist.char)
  def int = spawn(Dist.int)
  def float = spawn(Dist.float)
  def long = spawn(Dist.long)
  def double = spawn(Dist.double)
}

abstract class Random[A, G <: Generator](val dist: Dist[A]) { self =>

  def companion: RandomCompanion[G]

  def map[B](f: A => B): Random[B, G] =
    companion.spawn(dist.map(f))

  def flatMap[B](f: A => Random[B, G]): Random[B, G] =
    companion.spawn(dist.flatMap(gen => f(gen).dist))

  def run(): A =
    dist(companion.initGenerator) //IO

  def run(seed: Seed): A = { //IO
    val gen = companion.initGenerator()
    gen.setSeedBytes(seed.bytes)
    dist(gen)
  }

  def option: Random[Option[A], G] =
    companion.boolean.flatMap { b =>
      if (b) map(Some(_)) else companion.constant(None)
    }

  def listOfSize(n: Int): Random[List[A], G] =
    if (n > 0) listOfSize(n - 1).flatMap(as => map(_ :: as))
    else companion.constant(Nil)

  def list(n: Int): Random[List[A], G] =
    companion.int(n).flatMap(listOfSize)
}

object RandomLcg64 extends RandomCompanion[gen.Lcg64] {
  def initGenerator() =  gen.Lcg64.fromTime()
  def spawn[B](d: Dist[B]) = RandomLcg64(d)
}

case class RandomLcg64[A](d: Dist[A]) extends Random[A, gen.Lcg64](d) {
  def companion = RandomLcg64
}

object RandomSerial extends RandomCompanion[gen.Serial] {
  def initGenerator() =  new gen.Serial(0L)
  def spawn[B](d: Dist[B]) = RandomSerial(d)
}

case class RandomSerial[A](d: Dist[A]) extends Random[A, gen.Serial](d) {
  def companion = RandomSerial
}

case class RandomCmwc5[A](d: Dist[A]) extends Random[A, gen.Cmwc5](d) {
  def companion = Random
}

class Seed private[spire] (private[spire] val bytes: Array[Byte])

object Seed {
  val zero = Seed(Array[Byte](0, 0, 0, 0))
  def apply(n: Int): Seed = new Seed(spire.util.Pack.intToBytes(n))
  def apply(n: Long): Seed = new Seed(spire.util.Pack.longToBytes(n))
  def apply(bytes: Array[Byte]): Seed = new Seed(bytes.clone)
}

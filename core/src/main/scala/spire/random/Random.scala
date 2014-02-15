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

class Seed private[spire] (private[spire] val bytes: Array[Byte])

object Seed {
  def zero = Seed(Array[Byte](0, 0, 0, 0))
  def apply(n: Int): Seed = new Seed(spire.util.Pack.intToBytes(n))
  def apply(n: Long): Seed = new Seed(spire.util.Pack.longToBytes(n))
  def apply(bytes: Array[Byte]): Seed = new Seed(bytes.clone)
}

trait RandomCompanion[G <: mutable.Generator] {
  def initGenerator(): G //IO

  def generatorFromSeed(seed: Seed): G = {
    val gen = initGenerator()
    gen.setSeedBytes(seed.bytes)
    gen
  }

  def spawn[B](d: Dist[B]): Random[B, G]
}

abstract class Random[A, G <: mutable.Generator](val dist: Dist[A]) { self =>

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
}

object RandomLcg64 extends RandomCompanion[mutable.Lcg64] {
  def initGenerator() =  mutable.Lcg64.fromTime()
  def spawn[B](d: Dist[B]) = RandomLcg64(d)
}

case class RandomLcg64[A](d: Dist[A]) extends Random[A, mutable.Lcg64](d) {
  def companion = RandomLcg64
}

object RandomSerial extends RandomCompanion[mutable.Serial] {
  def initGenerator() =  new mutable.Serial(0L)
  def spawn[B](d: Dist[B]) = RandomSerial(d)
}

case class RandomSerial[A](d: Dist[A]) extends Random[A, mutable.Serial](d) {
  def companion = RandomSerial
}

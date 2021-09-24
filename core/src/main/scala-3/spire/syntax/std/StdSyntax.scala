package spire
package syntax
package std

import spire.algebra.{AdditiveMonoid, Field, Monoid, MultiplicativeMonoid, NRoot, Order, PartialOrder, Signed}
import spire.math.{Natural, Number, QuickSort, SafeLong, Searching, ULong}
import scala.collection.Factory
import spire.syntax.cfor._
import spire.syntax.monoid._
import spire.syntax.field._
import spire.syntax.nroot._
import spire.syntax.signed._


trait IntSyntax:
  extension(lhs: Int)
    def /~(rhs: Int): Int = lhs / rhs
    def /%(rhs: Int): (Int, Int) = (lhs / rhs, lhs % rhs)
    def pow(rhs: Int): Int = Math.pow(lhs, rhs).toInt
    def **(rhs: Int): Int = Math.pow(lhs, rhs).toInt
    def unary_! : BigInt = spire.math.fact(lhs)
    def choose(rhs: Int): BigInt = spire.math.choose(lhs, rhs)
end IntSyntax

trait LongSyntax:
  extension(lhs: Long)
    def /~(rhs: Long): Long = lhs / rhs
    def /%(rhs: Long): (Long, Long) = (lhs / rhs, lhs % rhs)
    def pow(rhs: Long): Long = spire.math.pow(lhs, rhs)
    def **(rhs: Long): Long = spire.math.pow(lhs, rhs)
    def unary_! : BigInt = spire.math.fact(lhs)
    def choose(rhs: Long): BigInt = spire.math.choose(lhs, rhs)
end LongSyntax

trait DoubleSyntax: 
  extension(lhs: Double)
    def pow(rhs: Double): Double = spire.math.pow(lhs, rhs)
    def **(rhs: Double): Double = spire.math.pow(lhs, rhs)
end DoubleSyntax

trait BigIntSyntax:
  extension(lhs: BigInt)
    def /~(rhs: BigInt): BigInt = lhs / rhs
    def pow(rhs: BigInt): BigInt = spire.math.pow(lhs, rhs)
    def **(rhs: BigInt): BigInt = spire.math.pow(lhs, rhs)

    def +(rhs: SafeLong): SafeLong = SafeLong(lhs) + rhs
    def *(rhs: SafeLong): SafeLong = SafeLong(lhs) * rhs
    def -(rhs: SafeLong): SafeLong = SafeLong(lhs) - rhs
    def /(rhs: SafeLong): SafeLong = SafeLong(lhs) / rhs
    def /~(rhs: SafeLong): SafeLong = SafeLong(lhs) /~ rhs
    def %(rhs: SafeLong): SafeLong = SafeLong(lhs) % rhs
    def /%(rhs: SafeLong): (SafeLong, SafeLong) = SafeLong(lhs) /% rhs

    def +(rhs: Natural): BigInt = lhs + rhs.toBigInt
    def *(rhs: Natural): BigInt = lhs * rhs.toBigInt
    def -(rhs: Natural): BigInt = lhs - rhs.toBigInt
    def /(rhs: Natural): BigInt = lhs / rhs.toBigInt
    def /~(rhs: Natural): BigInt = lhs / rhs.toBigInt
    def %(rhs: Natural): BigInt = lhs % rhs.toBigInt
    def /%(rhs: Natural): (BigInt, BigInt) = lhs /% rhs.toBigInt

    def +(rhs: ULong): BigInt = lhs + rhs.toBigInt
    def *(rhs: ULong): BigInt = lhs * rhs.toBigInt
    def -(rhs: ULong): BigInt = lhs - rhs.toBigInt
    def /(rhs: ULong): BigInt = lhs / rhs.toBigInt
    def /~(rhs: ULong): BigInt = lhs / rhs.toBigInt
    def %(rhs: ULong): BigInt = lhs % rhs.toBigInt
    def /%(rhs: ULong): (BigInt, BigInt) = lhs /% rhs.toBigInt

    def +(rhs: Number): Number = Number(lhs) + rhs
    def *(rhs: Number): Number = Number(lhs) * rhs
    def -(rhs: Number): Number = Number(lhs) - rhs
    def /(rhs: Number): Number = Number(lhs) / rhs
    def /~(rhs: Number): Number = Number(lhs) / rhs
    def %(rhs: Number): Number = Number(lhs).emod(rhs)
    def /%(rhs: Number): (Number, Number) = Number(lhs).equotmod(rhs)
end BigIntSyntax

trait ArraySyntax {
  implicit def arrayOps[@sp A](lhs: Array[A]): ArrayOps[A] = new ArrayOps(lhs)
}

trait SeqSyntax {
  implicit def seqOps[@sp A, CC[A] <: Iterable[A]](lhs: CC[A]): SeqOps[A, CC] = new SeqOps[A, CC](lhs)
  implicit def indexedSeqOps[@sp A, CC[A] <: IndexedSeq[A]](lhs: CC[A]): IndexedSeqOps[A, CC] =
    new IndexedSeqOps[A, CC](lhs)
}

package spire.syntax.std

import spire.math.ConvertableTo
import scala.{specialized => spec}

trait IntSyntax {
  implicit def literalIntOps(n: Int) = new LiteralIntOps(n)
  implicit def intToA[A](n:Int)(implicit c:ConvertableTo[A]): A = c.fromInt(n)
}

trait LongSyntax {
  implicit def literalLongOps(n: Long) = new LiteralLongOps(n)
}

trait DoubleSyntax {
  implicit def literalDoubleOps(n: Double) = new LiteralDoubleOps(n)
}

trait BigIntSyntax {
  implicit def literalBigIntOps(b: BigInt) = new LiteralBigIntOps(b)
}

trait ArraySyntax {
  implicit def arrayOps[@spec A](lhs:Array[A]) = new ArrayOps(lhs)
}

trait SeqSyntax {
  implicit def seqOps[@spec A, CC[A] <: Iterable[A]](lhs:CC[A]) = new SeqOps[A, CC](lhs)
  implicit def indexedSeqOps[@spec A, CC[A] <: IndexedSeq[A]](lhs:CC[A]) = new IndexedSeqOps[A, CC](lhs)
}


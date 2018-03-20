package spire
package syntax
package std

import spire.math.ConvertableTo

trait IntSyntax {
  implicit def literalIntOps(n: Int): LiteralIntOps = new LiteralIntOps(n)
  implicit def intToA[A](n:Int)(implicit c:ConvertableTo[A]): A = c.fromInt(n)
}

trait LongSyntax {
  implicit def literalLongOps(n: Long): LiteralLongOps = new LiteralLongOps(n)
}

trait DoubleSyntax {
  implicit def literalDoubleOps(n: Double): LiteralDoubleOps = new LiteralDoubleOps(n)
}

trait BigIntSyntax {
  implicit def literalBigIntOps(b: BigInt): LiteralBigIntOps = new LiteralBigIntOps(b)
}

trait ArraySyntax {
  implicit def arrayOps[@sp A](lhs:Array[A]): ArrayOps[A] = new ArrayOps(lhs)
}

trait SeqSyntax {
  implicit def seqOps[@sp A, CC[A] <: Iterable[A]](lhs:CC[A]): SeqOps[A, CC] = new SeqOps[A, CC](lhs)
  implicit def indexedSeqOps[@sp A, CC[A] <: IndexedSeq[A]](lhs:CC[A]): IndexedSeqOps[A, CC] = new IndexedSeqOps[A, CC](lhs)
}


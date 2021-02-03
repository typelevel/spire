package spire.math.extras.interval

import org.scalacheck.{Arbitrary, Gen}
import spire.implicits._

object IntervalSeqArbitrary {

  private def makeProfileXor(initial: Boolean, support: Array[Int], kind: Array[Int]): IntervalSeq[Int] = {
    require(support.length == kind.length)
    require(kind.forall(x => x >= 0 && x <= 2))
    def fromKind(x: Int, k: Int) = k match {
      case 0 => IntervalSeq.point(x)
      case 1 => IntervalSeq.above(x)
      case 2 => IntervalSeq.atOrAbove(x)
    }
    val r = IntervalSeq[Int](initial)
    support.zip(kind).foldLeft(r) { case (current, (x, k)) =>
      current ^ fromKind(x, k)
    }
  }

  private def randomProfileXor(min: Int, max: Int, count: Int): Gen[IntervalSeq[Int]] = {
    for {
      initial <- Gen.oneOf(true, false)
      edges <- Gen.resize(count, Gen.containerOf[Array, Int](Gen.choose(min, max)))
      support = edges.sorted.distinct
      kind <- Gen.containerOfN[Array, Int](support.length, Gen.oneOf(0, 1, 2))
    } yield makeProfileXor(initial, support, kind)
  }

  private def randomProfileGen(size: Int) = Gen.frequency[IntervalSeq[Int]](
    1 -> IntervalSeq.empty[Int],
    1 -> IntervalSeq.all[Int],
    15 -> randomProfileXor(0, 100, size),
    15 -> randomProfileXor(Int.MinValue, Int.MaxValue, size)
  )

  implicit val arbIntervalSeq = Arbitrary[IntervalSeq[Int]](randomProfileGen(3))
}

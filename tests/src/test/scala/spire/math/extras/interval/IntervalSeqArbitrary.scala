package spire.math.extras.interval

import org.scalacheck.{Arbitrary, Gen}
import spire.implicits._

object IntervalSeqArbitrary {

  private def makeProfileXor(initial:Boolean, support:Array[Long], kind:Array[Int]) : IntervalSeq[Long] = {
    require(support.length == kind.length)
    require(kind.forall(x => x >= 0 && x <= 2))
    def fromKind(x:Long, k:Int) = k match {
      case 0 => IntervalSeq.point(x)
      case 1 => IntervalSeq.above(x)
      case 2 => IntervalSeq.atOrAbove(x)
    }
    val r = IntervalSeq[Long](initial)
    (r /: (support zip kind)) {
      case (current, (x,k)) => current ^ fromKind(x,k)
    }
  }

  private def randomProfileXor(min: Long, max: Long, count: Int): Gen[IntervalSeq[Long]] = {
    for {
      initial <- Gen.oneOf(true, false)
      edges <- Gen.resize(count, Gen.containerOf[Array, Long](Gen.choose(min, max)))
      support = edges.sorted.distinct
      kind <- Gen.containerOfN[Array, Int](support.length, Gen.oneOf(0, 1, 2))
    } yield
      makeProfileXor(initial, support, kind)
  }

  private def randomProfileGen(size:Int) = Gen.frequency[IntervalSeq[Long]](
    1 -> IntervalSeq.empty[Long],
    1 -> IntervalSeq.all[Long],
    15 -> randomProfileXor(0, 100, size),
    15 -> randomProfileXor(Long.MinValue, Long.MaxValue, size)
  )

  implicit val arbIntervalSeq = Arbitrary[IntervalSeq[Long]](randomProfileGen(3))
}

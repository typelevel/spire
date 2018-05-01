package spire.math.extras.interval

import org.scalacheck.{Arbitrary, Gen}

object IntervalTrieArbitrary {

  def makeProfileXor(initial:Boolean, support:Array[Long], kind:Array[Int]) : IntervalTrie[Long] = {
    require(support.length == kind.length)
    require(kind.forall(x => x >= 0 && x <= 2))
    val r = IntervalTrie.constant[Long](initial)
    (support zip kind).foldLeft(r) {
      case (current, (x,k)) => current ^ IntervalTrie.fromKind(x,k)
    }
  }

  private def randomProfileXor(min: Long, max: Long, count: Int): Gen[IntervalTrie[Long]] = {
    for {
      initial <- Gen.oneOf(true, false)
      edges <- Gen.resize(count, Gen.containerOf[Array, Long](Gen.choose(min, max)))
      support = edges.sorted.distinct
      kind <- Gen.containerOfN[Array, Int](support.length, Gen.oneOf(0, 1, 2))
    } yield
      makeProfileXor(initial, support, kind)
  }

  private def randomProfileGen(size:Int) = Gen.frequency[IntervalTrie[Long]](
    1 -> IntervalTrie.empty[Long],
    1 -> IntervalTrie.all[Long],
    15 -> randomProfileXor(0, 100, size),
    15 -> randomProfileXor(Long.MinValue, Long.MaxValue, size)
  )

  implicit val arbIntervalTrie = Arbitrary[IntervalTrie[Long]](randomProfileGen(3))
}

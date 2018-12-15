package spire
package benchmark
/*
import spire.implicits._

object RandomBenchmarks extends MyRunner(classOf[RandomBenchmarks])

class RandomBenchmarks extends MyBenchmark with BenchmarkData {
  def intsToLong(x: Int, y: Int): Long = ((x & 0xffffffffL) << 32) | (y & 0xffffffffL)

  val ints16: Array[Int] = Array(
    -269317507, 81307275, 584521692, 2079271747,
    -1808083612, 1064281324, -1151709564, -1703051719,
    977240891, -1623897167, 368815549, 358018285,
    1735909162, 1296698489, -957499524, 1879467842
  )

  val ints4: Array[Int] = Array(ints16(0), ints16(1), ints16(2), ints16(3))

  val int: Int = ints16(0)
  val long: Long = intsToLong(ints16(0), ints16(1))

  val longs5: Array[Long] = Array(
    intsToLong(ints16(0), ints16(1)),
    intsToLong(ints16(2), ints16(3)),
    intsToLong(ints16(4), ints16(5)),
    intsToLong(ints16(6), ints16(7)),
    intsToLong(ints16(8), ints16(9))
  )

  val longs16: Array[Long] = Array(
    intsToLong(ints16(0), ints16(1)),
    intsToLong(ints16(1), ints16(2)),
    intsToLong(ints16(2), ints16(3)),
    intsToLong(ints16(3), ints16(4)),
    intsToLong(ints16(4), ints16(5)),
    intsToLong(ints16(5), ints16(6)),
    intsToLong(ints16(6), ints16(7)),
    intsToLong(ints16(7), ints16(8)),
    intsToLong(ints16(8), ints16(9)),
    intsToLong(ints16(9), ints16(10)),
    intsToLong(ints16(10), ints16(11)),
    intsToLong(ints16(11), ints16(12)),
    intsToLong(ints16(12), ints16(13)),
    intsToLong(ints16(13), ints16(14)),
    intsToLong(ints16(14), ints16(15)),
    intsToLong(ints16(15), ints16(0))
  )

  val javaRng = new java.util.Random(long)
  val scalaRng = new scala.util.Random(long)
  val lcg32Rng = spire.random.rng.Lcg32.fromSeed(int)
  val lcg64Rng = spire.random.rng.Lcg64.fromSeed(long)
  val burtle2Rng = spire.random.rng.BurtleRot2.fromSeed(ints4)
  val burtle3Rng = spire.random.rng.BurtleRot3.fromSeed(ints4)
  val cmwc5Rng = spire.random.rng.Cmwc5.fromSeed(longs5)
  val well512aRng = spire.random.rng.Well512a.fromSeed((ints16, 0))
  val well1024aRng = spire.random.rng.Well1024a.fromArray(ints16)
  val well19937aRng = spire.random.rng.Well19937a.fromArray(ints16)
  val well19937cRng = spire.random.rng.Well19937c.fromArray(ints16)
  val well44497aRng = spire.random.rng.Well44497a.fromArray(ints16)
  val well44497bRng = spire.random.rng.Well44497b.fromArray(ints16)
  val xorShift64StarRng = spire.random.rng.extras.XorShift64Star.fromSeed(long)
  val xorShift1024StarRng = spire.random.rng.extras.XorShift1024Star.fromSeed((longs16, 0))
  val xorShift128PlusRng = spire.random.rng.extras.XorShift128Plus.fromSeed((long, long))

  @inline final def nextLen = 1000000

  // nextInt()
  def timeNextIntJava(reps: Int) = run(reps) {
    val rng = javaRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntScala(reps: Int) = run(reps) {
    val rng = scalaRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntLcg32(reps: Int) = run(reps) {
    val rng = lcg32Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntLcg64(reps: Int) = run(reps) {
    val rng = lcg64Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntBurtleRot2(reps: Int) = run(reps) {
    val rng = burtle2Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntBurtleRot3(reps: Int) = run(reps) {
    val rng = burtle3Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntCmwc5(reps: Int) = run(reps) {
    val rng = cmwc5Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntWell512a(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntWell1024a(reps: Int) = run(reps) {
    val rng = well1024aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntWell19937a(reps: Int) = run(reps) {
    val rng = well19937aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntWell19937c(reps: Int) = run(reps) {
    val rng = well19937cRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntWell44497a(reps: Int) = run(reps) {
    val rng = well44497aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntWell44497b(reps: Int) = run(reps) {
    val rng = well44497bRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntXorShift64Star(reps: Int) = run(reps) {
    val rng = xorShift64StarRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntXorShift1024Star(reps: Int) = run(reps) {
    val rng = xorShift1024StarRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntXorShift128Plus(reps: Int) = run(reps) {
    val rng = xorShift128PlusRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  // synchronous nextInt()
  def timeNextIntSyncJava(reps: Int) = run(reps) {
    val rng = javaRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncScala(reps: Int) = run(reps) {
    val rng = scalaRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncLcg32(reps: Int) = run(reps) {
    val rng = lcg32Rng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncLcg64(reps: Int) = run(reps) {
    val rng = lcg64Rng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncBurtleRot2(reps: Int) = run(reps) {
    val rng = burtle2Rng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncBurtleRot3(reps: Int) = run(reps) {
    val rng = burtle3Rng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncCmwc5(reps: Int) = run(reps) {
    val rng = cmwc5Rng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncWell512a(reps: Int) = run(reps) {
    val rng = well512aRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncWell1024a(reps: Int) = run(reps) {
    val rng = well1024aRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncWell19937a(reps: Int) = run(reps) {
    val rng = well19937aRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncWell19937c(reps: Int) = run(reps) {
    val rng = well19937cRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncWell44497a(reps: Int) = run(reps) {
    val rng = well44497aRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncWell44497b(reps: Int) = run(reps) {
    val rng = well44497bRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncXorShift64Star(reps: Int) = run(reps) {
    val rng = xorShift64StarRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncXorShift1024Star(reps: Int) = run(reps) {
    val rng = xorShift1024StarRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  def timeNextIntSyncXorShift128Plus(reps: Int) = run(reps) {
    val rng = xorShift128PlusRng.sync
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt())
  }

  // nextLong()
  def timeNextLongJava(reps: Int) = run(reps) {
    val rng = javaRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongScala(reps: Int) = run(reps) {
    val rng = scalaRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongLcg32(reps: Int) = run(reps) {
    val rng = lcg32Rng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongLcg64(reps: Int) = run(reps) {
    val rng = lcg64Rng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongBurtleRot2(reps: Int) = run(reps) {
    val rng = burtle2Rng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongBurtleRot3(reps: Int) = run(reps) {
    val rng = burtle3Rng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongCmwc5(reps: Int) = run(reps) {
    val rng = cmwc5Rng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongWell512a(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongWell1024a(reps: Int) = run(reps) {
    val rng = well1024aRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongWell19937a(reps: Int) = run(reps) {
    val rng = well19937aRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongWell19937c(reps: Int) = run(reps) {
    val rng = well19937cRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongWell44497a(reps: Int) = run(reps) {
    val rng = well44497aRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongWell44497b(reps: Int) = run(reps) {
    val rng = well44497bRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongXorShift64Star(reps: Int) = run(reps) {
    val rng = xorShift64StarRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongXorShift1024Star(reps: Int) = run(reps) {
    val rng = xorShift1024StarRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  def timeNextLongXorShift128Plus(reps: Int) = run(reps) {
    val rng = xorShift128PlusRng
    var t = 0L
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextLong())
  }

  // nextDouble()
  def timeNextDoubleJava(reps: Int) = run(reps) {
    val rng = javaRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleScala(reps: Int) = run(reps) {
    val rng = scalaRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleLcg32(reps: Int) = run(reps) {
    val rng = lcg32Rng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleLcg64(reps: Int) = run(reps) {
    val rng = lcg64Rng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleBurtleRot2(reps: Int) = run(reps) {
    val rng = burtle2Rng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleBurtleRot3(reps: Int) = run(reps) {
    val rng = burtle3Rng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleCmwc5(reps: Int) = run(reps) {
    val rng = cmwc5Rng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleWell512a(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleWell1024a(reps: Int) = run(reps) {
    val rng = well1024aRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleWell19937a(reps: Int) = run(reps) {
    val rng = well19937aRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleWell19937c(reps: Int) = run(reps) {
    val rng = well19937cRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleWell44497a(reps: Int) = run(reps) {
    val rng = well44497aRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleWell44497b(reps: Int) = run(reps) {
    val rng = well44497bRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleXorShift64Star(reps: Int) = run(reps) {
    val rng = xorShift64StarRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleXorShift1024Star(reps: Int) = run(reps) {
    val rng = xorShift1024StarRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  def timeNextDoubleXorShift128Plus(reps: Int) = run(reps) {
    val rng = xorShift128PlusRng
    var t = 0.0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextDouble())
  }

  // nextInt(100)
  def timeNextInt100Java(reps: Int) = run(reps) {
    val rng = javaRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Scala(reps: Int) = run(reps) {
    val rng = scalaRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Lcg32(reps: Int) = run(reps) {
    val rng = lcg32Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Lcg64(reps: Int) = run(reps) {
    val rng = lcg64Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100BurtleRot2(reps: Int) = run(reps) {
    val rng = burtle2Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100BurtleRot3(reps: Int) = run(reps) {
    val rng = burtle3Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Cmwc5(reps: Int) = run(reps) {
    val rng = cmwc5Rng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Well512a(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Well1024a(reps: Int) = run(reps) {
    val rng = well1024aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Well19937a(reps: Int) = run(reps) {
    val rng = well19937aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Well19937c(reps: Int) = run(reps) {
    val rng = well19937cRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Well44497a(reps: Int) = run(reps) {
    val rng = well44497aRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100Well44497b(reps: Int) = run(reps) {
    val rng = well44497bRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100XorShift64Star(reps: Int) = run(reps) {
    val rng = xorShift64StarRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100XorShift1024Star(reps: Int) = run(reps) {
    val rng = xorShift1024StarRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  def timeNextInt100XorShift128Plus(reps: Int) = run(reps) {
    val rng = xorShift128PlusRng
    var t = 0
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextInt(100))
  }

  // fillBytes(128)
  @inline final def fillLen = 100000

  def timeFillBytesJava(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = javaRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.nextBytes(bytes))
  }

  def timeFillBytesScala(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = scalaRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.nextBytes(bytes))
  }

  def timeFillBytesLcg32(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = lcg32Rng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesLcg64(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = lcg64Rng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesBurtleRot2(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = burtle2Rng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesBurtleRot3(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = burtle3Rng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesCmwc5(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = cmwc5Rng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesWell512a(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = well512aRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesWell1024a(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = well1024aRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesWell19937a(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = well19937aRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesWell19937c(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = well19937cRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesWell44497a(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = well44497aRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesWell44497b(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = well44497bRng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesXorShift64Star(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = xorShift64StarRng
    cfor(0)(_ < nextLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesXorShift1024Star(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = xorShift1024StarRng
    cfor(0)(_ < nextLen, _ + 1)(_ => rng.fillBytes(bytes))
  }

  def timeFillBytesXorShift128Plus(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = xorShift128PlusRng
    cfor(0)(_ < nextLen, _ + 1)(_ => rng.fillBytes(bytes))
  }
}
*/
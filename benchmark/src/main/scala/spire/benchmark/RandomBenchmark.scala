package spire.benchmark

import scala.{specialized => spec}
import scala.reflect.ClassTag

import spire.implicits._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

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

  val javaRng = new java.util.Random(long)
  val scalaRng = new scala.util.Random(long)
  val lcg32Rng = spire.random.mutable.Lcg32.fromSeed(int)
  val lcg64Rng = spire.random.mutable.Lcg64.fromSeed(long)
  val burtle2Rng = spire.random.mutable.BurtleRot2.fromSeed(ints4)
  val burtle3Rng = spire.random.mutable.BurtleRot3.fromSeed(ints4)
  val cmwc5Rng = spire.random.mutable.Cmwc5.fromSeed(longs5)
  val well512Rng = spire.random.mutable.Well512.fromSeed(ints16)

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

  def timeNextIntWell512(reps: Int) = run(reps) {
    val rng = well512Rng
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

  def timeNextIntSyncWell512(reps: Int) = run(reps) {
    val rng = well512Rng.sync
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

  def timeNextLongWell512(reps: Int) = run(reps) {
    val rng = well512Rng
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

  def timeNextDoubleWell512(reps: Int) = run(reps) {
    val rng = well512Rng
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

  def timeNextInt100Well512(reps: Int) = run(reps) {
    val rng = well512Rng
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

  def timeFillBytesWell512(reps: Int) = run(reps) {
    val bytes = new Array[Byte](128)
    val rng = well512Rng
    cfor(0)(_ < fillLen, _ + 1)(_ => rng.fillBytes(bytes))
  }
}

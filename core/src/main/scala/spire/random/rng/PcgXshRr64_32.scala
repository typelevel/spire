package spire
package random
package rng

import java.util.concurrent.atomic.AtomicLong

import scala.annotation.tailrec
import spire.syntax.literals._
import spire.util.Pack

/**
 * This is a Scala implementation of the PCG-XSH-RR-64/32 PRNG based on <a href="https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c">basic C implementation</a>.
 *
 * <p><b>Reference: </b>
 * Melissa E. O'Neill:
 * <a href="http://www.pcg-random.org/paper.html">"PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation"</a>,
 * Submitted to <i>ACM Transactions on Mathematical Software</i>.
 *
 * @see <a href="http://www.pcg-random.org">PCG Home Page</a>
 * @author <a href="mailto:alexey.v.romanov@gmail.com">Alexey Romanov</a>
 */
class PcgXshRr64_32 private (private var state: Long, private var inc: Long) extends IntBasedGenerator {
  protected[this] def copyInit = new PcgXshRr64_32(state, inc)

  def nextInt() = {
    val oldState = state

    state = oldState * 6364136223846793005L + inc
    val xorShifted = (((oldState >>> 18) ^ oldState) >>> 27).toInt
    val rot = (oldState >>> 59).toInt
    Integer.rotateRight(xorShifted, rot)
  }

  def seed(initState: Long, initSeq: Long): Unit = {
    state = 0L
    inc = (initSeq << 1) | 1L
    nextInt()
    state += initState
    nextInt()
  }

  def seed(seed: PcgSeed64): Unit = this.seed(seed.initState, seed.initSeq)

  override def getSeedBytes(): Array[Byte] =
    Pack.longsToBytes(Array(state, inc))

  override def setSeedBytes(bytes: Array[Byte]): Unit = {
    val longs = Pack.longsFromBytes(bytes, 2)
    state = longs(0)
    inc = longs(1)
  }
}

object PcgXshRr64_32 extends GeneratorCompanion[PcgXshRr64_32, PcgSeed64] {
  override def randomSeed(): PcgSeed64 =
    PcgSeed64(System.nanoTime(), nextStreamId())

  override def fromTime(time: Long = System.nanoTime()): PcgXshRr64_32 =
    fromSeed(PcgSeed64(time, nextStreamId()))

  override def fromSeed(seed: PcgSeed64): PcgXshRr64_32 = {
    val gen = new PcgXshRr64_32(0L, 0L)
    gen.seed(seed)
    gen
  }

  override def fromBytes(bytes: Array[Byte]): PcgXshRr64_32 = {
    val longs = Pack.longsFromBytes(bytes, 2)
    fromSeed(PcgSeed64(longs(0), longs(1)))
  }

  private[this] val streamUniquifier = new AtomicLong(System.identityHashCode(PcgXshRr64_32))

  @tailrec
  private[this] def nextStreamId(): Long = {
    val current = streamUniquifier.get()
    val next = current * 181783497276652981L
    if (streamUniquifier.compareAndSet(current, next)) {
      next
    } else {
      nextStreamId()
    }
  }
}

case class PcgSeed64(initState: Long, initSeq: Long)

package spire.random
package rng

import java.nio.ByteBuffer
import java.util.Arrays
import java.security.SecureRandom

class SecureJava(rand: SecureRandom) extends IntBasedGenerator {
  def copyInit: SecureJava = new SecureJava(rand)

  def getSeedBytes: Array[Byte] =
    throw new UnsupportedOperationException("getSeedBytes")

  def setSeedBytes(bytes: Array[Byte]): Unit =
    throw new UnsupportedOperationException("setSeedBytes")

  def nextInt(): Int = rand.nextInt()
}

object SecureJava {
  def fromBytes(bytes: Array[Byte]): SecureJava =
    new SecureJava(new SecureRandom(bytes))

  def apply(): SecureJava =
    new SecureJava(new SecureRandom())
}

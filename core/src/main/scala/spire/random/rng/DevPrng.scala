package spire
package random
package rng

import java.io._

class Device(f: File) extends Generator { self =>
  if (!f.canRead)
    throw new IllegalArgumentException("can't read %s".format(f))

  private val dis = new DataInputStream(new FileInputStream(f))

  def copyInit: Generator = new Device(f)

  def getSeedBytes(): Array[Byte] =
    throw new UnsupportedOperationException("getSeedBytes")

  def setSeedBytes(bytes: Array[Byte]): Unit =
    throw new UnsupportedOperationException("setSeedBytes")

  def nextInt(): Int = dis.readInt()

  def nextLong(): Long = dis.readLong()
}

object Device {
  def apply(path: String): Device = new Device(new File(path))
  def random: Device = new Device(new File("/dev/random"))
  def urandom: Device = new Device(new File("/dev/urandom"))
}

class CycledFile(f: File) extends Generator { self =>
  private var dis: DataInputStream = null

  if (!f.canRead)
    throw new IllegalArgumentException("can't read %s".format(f))
  else
    reinit()

  try {
    nextLong()
  } catch {
    case e: EOFException =>
      throw new IllegalArgumentException("%s contains less than 8 bytes".format(f))
  }

  def reinit(): Unit = {
    if (dis != null) dis.close()
    dis = new DataInputStream(new FileInputStream(f))
  }

  def copyInit: Generator = new CycledFile(f)

  def getSeedBytes(): Array[Byte] =
    throw new UnsupportedOperationException("getSeedBytes")

  def setSeedBytes(bytes: Array[Byte]): Unit =
    throw new UnsupportedOperationException("setSeedBytes")

  def nextInt(): Int = try {
    dis.readInt()
  } catch {
    case e: EOFException =>
      reinit()
      dis.readInt()
  }

  def nextLong(): Long = try {
    dis.readLong()
  } catch {
    case e: EOFException =>
      reinit()
      dis.readInt()
  }
}

object CycledFile {
  def apply(path: String): CycledFile = new CycledFile(new File(path))
}

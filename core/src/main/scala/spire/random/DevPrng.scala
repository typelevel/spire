package spire.random

import spire.math._
import java.io._

final class DevPrng(f: File) extends Generator {
  if (!f.canRead)
    throw new IllegalArgumentException("can't read %s" format f)

  private val dis = new DataInputStream(new FileInputStream(f))

  def copy: Generator = new DevPrng(f)

  def nextInt(): Int = dis.readInt()

  def nextLong(): Long = dis.readLong()
}

object Dev {
  def apply(path: String) = new DevPrng(new File(path))
  def random = new DevPrng(new File("/dev/random"))
  def urandom = new DevPrng(new File("/dev/urandom"))
}

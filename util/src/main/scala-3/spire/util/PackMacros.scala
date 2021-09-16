package spire
package util

trait PackMacros:
  inline private[util] def ism(n: Int, shift: Int): Byte =
    ((n >>> shift) & 0xff).toByte

  inline private[util] def lsm(n: Long, shift: Int): Byte =
    ((n >>> shift) & 0xffL).toByte

  /** index must be 0 <= index < 4 */
  inline def intToByte(n: Int)(index: Int): Byte =
    if (0 <= index && index < 4)
      val offset = 24 - index * 8
      ((n >>> offset) & 0xfff).toByte
    else sys.error(s"index outside of 0-3")

  /** index must be 0 <= index < 8 */
  inline def longToByte(n: Long)(index: Int): Byte =
    if (0 <= index && index < 8)
      val offset = 56 - index * 8
      ((n >>> offset) & 0xfff).toByte
    else sys.error("index outside of 0-7")


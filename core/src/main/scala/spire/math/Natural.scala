package spire.math

import spire.algebra._

import scala.annotation.tailrec
import scala.{specialized => spec}

import Natural._

// NOTE: this class works, but is only optimal for a relatively narrow
// set of problems. for really big numbers you're probably better off
// using SafeLong or BigInt. that said, there are cases where Natural
// is faster (e.g. addition with 32-128 bit numbers).

// TODO: almost none of this recursion is tailrec. the first goal was
// correctness, but once that's achieved we need to focus on efficiency.
// using a similar "private mutable" strategy that :: and ListBuffer
// use in Scala, we should be able to efficiently build Digit chains
// in a tail-recursive way.

sealed trait Natural {
  lhs =>

  def digit: UInt

  def foldDigitsLeft[@spec A](a: A)(f: (A, UInt) => A): A = {
    @tailrec def recur(next: Natural, sofar: A): A = next match {
      case End(d) => f(a, d)
      case Digit(d, tail) => recur(tail, f(a, d))
    }
    recur(this, a)
  }


  def foldDigitsRight[@spec A](a: A)(f: (A, UInt) => A): A =
    reversed.foldDigitsLeft(a)(f)

  def getNumBits: Int = {
    @tailrec
    def bit(n: UInt, b: Int): Int = if (n == 0) b else bit(n >>> 1, b + 1)

    @tailrec
    def recur(next: Natural, b: Int): Int = next match {
      case End(d) => b + bit(d, 0)
      case Digit(_, tail) => recur(tail, b + 32)
    }
    recur(this, 0)
  }

  def getDigitLength: Int = {
    @tailrec
    def recur(next: Natural, n: Int): Int = next match {
      case End(d) => n + 1
      case Digit(d, tail) => recur(tail, n + 1)
    }
    recur(this, 0)
  }

  def toList: List[UInt] = {
    @tailrec
    def recur(next: Natural, sofar: List[UInt]): List[UInt] = next match {
      case End(d) => d :: sofar
      case Digit(d, tail) => recur(tail, d :: sofar)
    }
    recur(this, Nil)
  }

  // Array[UInt] would be boxed so we do this for now.
  def toArray: Array[Int] = {
    val n = getDigitLength
    val arr = new Array[Int](n)
    @tailrec
    def recur(next: Natural, i: Int): Unit = next match {
      case End(d) =>
        arr(i) = d.signed
      case Digit(d, tail) =>
        arr(i) = d.signed
        recur(tail, i - 1)
    }
    recur(this, n - 1)
    arr
  }

  def reversed: Natural = {
    @tailrec
    def recur(next: Natural, sofar: Natural): Natural = next match {
      case End(d) => Digit(d, sofar)
      case Digit(d, tail) => recur(tail, Digit(d, sofar))
    }
    this match {
      case Digit(d, tail) => recur(tail, End(d))
      case _ => this
    }
  }

  def trim: Natural = {
    @tailrec
    def recur(next: Natural): Natural = {
      next match {
        case Digit(n, tail) =>
          if (n == UInt(0)) recur(tail) else next
        case End(n) =>
          next
      }
    }
    recur(reversed).reversed
  }

  def toInt: Int = digit.toInt & 0x7fffffff

  def toLong: Long = this match {
    case End(d) => d.toLong
    case Digit(d, tail) => (tail.toLong << 32L) + d.toLong
  }

  def toBigInt: BigInt = this match {
    case End(d) => BigInt(d.toLong)
    case Digit(d, tail) => (tail.toBigInt << 32) + BigInt(d.toLong)
  }

  // calculate 9 digits at a time using /%
  override def toString: String = {
    @tailrec def recur(next: Natural, s: String): String = {
      next match {
        case End(d) =>
          d.toLong.toString + s
        case Digit(d, tail) =>
          val (q, r) = next /% Natural.denom
          if (q == UInt(0))
            r.digit.toLong.toString + s
          else
            recur(q, "%09d%s" format (r.digit.toLong, s))
      }
    }
    recur(this, "")
  }

  def toRepr: String = toList.mkString("Natural(", ", ", ")")

  def isZero: Boolean = {
    @tailrec
    def recur(next: Natural): Boolean = next match {
      case End(n) =>
        n == UInt(0)
      case Digit(n, tail) =>
        if (n == UInt(0)) recur(tail) else false
    }
    recur(this)
  }

  def isOne: Boolean = this match {
    case End(n) =>
      n == UInt(1)
    case Digit(n, tail) =>
      n == UInt(1) && tail.isZero
  }

  def isOdd: Boolean = (digit & UInt(1)) == UInt(1)

  def isEven: Boolean = (digit & UInt(1)) == UInt(0)

  def powerOfTwo: Int = {
    import java.lang.Integer.highestOneBit

    def test(n: UInt): Int = {
      if ((n.signed & -n.signed) != n.signed) return -1
      // TODO: this could be better/faster
      var i = 1
      while (i < 32 && (n >>> i) != 0) i += 1
      i - 1
    }

    @tailrec
    def recur(next: Natural, shift: Int, bit: Int): Int = next match {
      case End(n) =>
        val t = test(n)
        if (t < 0) -1 else if (bit < 0) shift + t else -1
      case Digit(n, tail) =>
        val t = test(n)
        if (t < 0)
          recur(tail, shift + 32, bit)
        else if (bit < 0)
          recur(tail, shift + 32, shift + t)
        else
          -1
    }
    recur(this, 0, -1)
  }

  def compare(rhs: UInt): Int = this match {
    case End(d) =>
      if (d < rhs) -1 else if (d > rhs) 1 else 0
    case Digit(d, tail) =>
      if (tail.isZero)
        if (d > rhs) 1 else if (d < rhs) -1 else 0
      else
        1
  }

  def compare(rhs: Natural): Int = {
    def cmp(a: UInt, b: UInt, c: Int): Int =
      if (a < b) -1 else if (a > b) 1 else c

    @tailrec
    def recur(lhs: Natural, rhs: Natural, d: Int): Int = lhs match {
      case End(ld) => rhs match {
        case End(rd) => cmp(ld, rd, d)
        case _: Digit => -rhs.compare(ld)
      }
      case Digit(ld, ltail) => rhs match {
        case End(rd) => lhs.compare(rd)
        case Digit(rd, rtail) => recur(ltail, rtail, cmp(ld, rd, d))
      }
    }

    recur(lhs, rhs, 0)
  }

  final override def equals(rhs: Any): Boolean = rhs match {
    case rhs: Natural => (lhs compare rhs) == 0
    case rhs: UInt => (lhs compare rhs) == 0
    case _ => false
  }

  def <(rhs: Natural): Boolean = (lhs compare rhs) < 0
  def <=(rhs: Natural): Boolean = (lhs compare rhs) <= 0
  def >(rhs: Natural): Boolean = (lhs compare rhs) > 0
  def >=(rhs: Natural): Boolean = (lhs compare rhs) >= 0

  def <(r: UInt): Boolean = (lhs compare r) < 0
  def <=(r: UInt): Boolean = (lhs compare r) <= 0
  def >(r: UInt): Boolean = (lhs compare r) > 0
  def >=(r: UInt): Boolean = (lhs compare r) >= 0
  def <(r: BigInt): Boolean = (lhs.toBigInt compare r) < 0
  def <=(r: BigInt): Boolean = (lhs.toBigInt compare r) <= 0
  def >(r: BigInt): Boolean = (lhs.toBigInt compare r) > 0
  def >=(r: BigInt): Boolean = (lhs.toBigInt compare r) >= 0

  // implemented in Digit and End
  def +(rd: UInt): Natural
  def -(rd: UInt): Natural
  def *(rd: UInt): Natural
  def /~(rd: UInt): Natural = lhs / rd
  def /(rd: UInt): Natural
  def %(rd: UInt): Natural
  def /%(rd: UInt): (Natural, Natural)

  def +(rhs: BigInt): BigInt = lhs.toBigInt + rhs
  def -(rhs: BigInt): BigInt = lhs.toBigInt - rhs
  def *(rhs: BigInt): BigInt = lhs.toBigInt * rhs
  def /~(rhs: BigInt): BigInt = lhs.toBigInt / rhs
  def /(rhs: BigInt): BigInt = lhs.toBigInt / rhs
  def %(rhs: BigInt): BigInt = lhs.toBigInt % rhs
  def /%(rhs: BigInt): (BigInt, BigInt) = lhs.toBigInt /% rhs

  def +(rhs: Natural): Natural = {
    def recur(left: Natural, right: Natural, carry: Long): Natural = left match {
      case End(ld) => right match {
        case End(rd) =>
          Natural(ld.toLong + rd.toLong + carry)
          
        case Digit(rd, rtail) =>
          val t = ld.toLong + rd.toLong + carry
          Digit(UInt(t), rtail + UInt(t >> 32))
      }

      case Digit(ld, ltail) => right match {
        case End(rd) =>
          val t = ld.toLong + rd.toLong + carry
          Digit(UInt(t), ltail + UInt(t >> 32))
          
        case Digit(rd, rtail) =>
          val t = ld.toLong + rd.toLong + carry
          Digit(UInt(t), recur(ltail, rtail, t >> 32))
      }
    }
    recur(lhs, rhs, 0L)
  }

  def -(rhs: Natural): Natural = {
    def recur(left: Natural, right: Natural, carry: Long): Natural = left match {
      case End(ld) => right match {
        case End(rd) =>
          Natural(ld.toLong - rd.toLong - carry)
          
        case Digit(rd, rtail) =>
          val t = ld.toLong - rd.toLong - carry
          val tl = rtail - UInt(-(t >> 32))
          if (tl.isInstanceOf[End] && tl.digit == UInt(0))
            End(UInt(t))
          else
            Digit(UInt(t), tl)
      }

      case Digit(ld, ltail) => right match {
        case End(rd) =>
          val t = ld.toLong - rd.toLong - carry
          val tl = ltail - UInt(-(t >> 32))
          if (tl.isInstanceOf[End] && tl.digit == UInt(0))
            End(UInt(t))
          else
            Digit(UInt(t), tl)
          
        case Digit(rd, rtail) =>
          val t = ld.toLong - rd.toLong - carry
          val tl = recur(ltail, rtail, -(t >> 32))
          if (tl.isInstanceOf[End] && tl.digit == UInt(0))
            End(UInt(t))
          else
            Digit(UInt(t), tl)
      }
    }
    if (lhs < rhs)
      throw new ArithmeticException("negative subtraction: %s - %s" format (lhs, rhs))
    else
      recur(lhs, rhs, 0L)
  }

  def *(rhs: Natural): Natural = lhs match {
    case End(ld) => rhs * ld
    case Digit(ld, ltail) => rhs match {
      case End(rd) => lhs * rd
      case Digit(rd, rtail) =>
        Digit(UInt(0), Digit(UInt(0), ltail * rtail)) +
        Digit(UInt(0), ltail * rd) +
        Digit(UInt(0), rtail * ld) +
        Natural(ld.toLong * rd.toLong)
    }
  }

  def pow(rhs: Natural): Natural = {
    @tailrec def _pow(t: Natural, b: Natural, e: Natural): Natural = {
      if (e.isZero) t
      else if (e.isOdd) _pow(t * b, b * b, e >> 1)
      else _pow(t, b * b, e >> 1)
    }
    _pow(Natural(1), lhs, rhs)
  }

  def pow(rhs: UInt): Natural = {
    @tailrec def _pow(t: Natural, b: Natural, e: UInt): Natural = {
      if (e == UInt(0)) t
      else if ((e & UInt(1)) == UInt(1)) _pow(t * b, b * b, e >> 1)
      else _pow(t, b * b, e >> 1)
    }
    _pow(Natural(1), lhs, rhs)
  }

  def /~(rhs: Natural): Natural = lhs / rhs

  def /(rhs: Natural): Natural = {
    rhs match {
      case End(rd) =>
        lhs / rd
  
      case Digit(rd, rtail) => lhs match {
        case End(ld) =>
          End(UInt(0))

        case Digit(ld, ltail) => rhs.compare(UInt(1)) match {
          case -1 => sys.error("/ by zero")
          case 0 =>
            lhs
          case 1 =>
            val p = rhs.powerOfTwo
            if (p >= 0) {
              lhs >> p
            } else {
              longdiv(lhs, rhs)._1
            }
        }
      }
    }
  }

  def %(rhs: Natural): Natural = {
    rhs match {
      case End(rd) => lhs % rd
  
      case Digit(rd, rtail) => lhs match {
        case End(ld) => End(ld)

        case Digit(ld, ltail) => rhs.compare(UInt(1)) match {
          case -1 => sys.error("/ by zero")
          case 0 => End(UInt(0))
          case 1 =>
            val p = rhs.powerOfTwo
            if (p >= 0)
              lhs & ((Natural(1) << p) - UInt(1))
            else
              longdiv(lhs, rhs)._2
        }
      }
    }
  }

  def /%(rhs: Natural): (Natural, Natural) = {
    rhs match {
      case End(rd) => (lhs / rd, lhs % rd)
  
      case Digit(rd, rtail) => lhs match {
        case End(ld) => (End(UInt(0)), lhs)

        case Digit(ld, ltail) => rhs.compare(UInt(1)) match {
          case -1 => sys.error("/ by zero")
          case 0 => (lhs, Natural(0))
          case 1 =>
            val p = rhs.powerOfTwo
            if (p >= 0) {
              val mask = (Natural(1) << p) - UInt(1)
              (lhs >> p, lhs & mask)
            } else {
              longdiv(lhs, rhs)
            }
        }
      }
    }
  }

  private def longdiv(num: Natural, denom: Natural): (Natural, Natural) = {
    var rem = num
    var quo = Natural(0)

    var remBits: Int = rem.getNumBits
    var denomBits: Int = denom.getNumBits
    var shift: Int = remBits - denomBits

    while (shift >= 0) {
      val shifted = denom << shift
      if (shifted <= rem) {
        quo += Natural(1) << shift
        rem -= shifted
        remBits = rem.getNumBits
        shift = remBits - denomBits
      } else {
        shift -= 1
      }
    }

    (quo, rem)
  }

  def <<(n: Int): Natural = {
    val m: Int = n & 0x1f
    def recur(next: Natural, carry: Long): Natural = next match {
      case End(d) =>
        Natural((d.toLong << m) | carry)
      case Digit(d, tail) =>
        val t = (d.toLong << m) | carry
        Digit(UInt(t), recur(tail, t >> 32))
    }
    val num = recur(this, 0L)
    (0 until n / 32).foldLeft(num)((n, _) => Digit(UInt(0), n))
  }

  def chop(n: Int): Natural = {
    @tailrec def recur(next: Natural, n: Int): Natural = if (n <= 0) {
      next
    } else {
      next match {
        case End(d) => End(UInt(0))
        case Digit(d, tail) => recur(tail, n - 1)
      }
    }
    recur(this, n)
  }

  def >>(n: Int): Natural = {
    val m: Int = n & 0x1f
    def recur(next: Natural, carry: Long): Natural = next match {
      case End(d) =>
        Natural((d.toLong >> m) | carry)
      case Digit(d, tail) =>
        val t = (d.toLong | carry) << (32 - m)
        Digit(UInt(t >> 32), recur(tail, t & 0xffffffffL))
    }
    recur(chop(n / 32).reversed, 0L).reversed
  }

  def |(rhs: Natural): Natural = lhs match {
    case End(ld) => rhs match {
      case End(rd) => End(ld | rd)
      case Digit(rd, rtail) => Digit(ld | rd, rtail)
    }
    case Digit(ld, ltail) => rhs match {
      case End(rd) => Digit(ld | rd, ltail)
      case Digit(rd, rtail) => Digit(ld | rd, ltail | rtail)
    }
  }

  def |(rhs: UInt): Natural = lhs match {
    case End(ld) => End(ld | rhs)
    case Digit(ld, ltail) => Digit(ld | rhs, ltail)
  }

  def &(rhs: Natural): Natural = {
    def and(lhs: Natural, rhs: Natural): Natural = lhs match {
      case End(ld) => rhs match {
        case End(rd) => End(ld & rd)
        case Digit(rd, rtail) => End(ld & rd)
      }
      case Digit(ld, ltail) => rhs match {
        case End(rd) => End(ld & rd)
        case Digit(rd, rtail) => Digit(ld & rd, and(ltail, rtail))
      }
    }
    and(lhs, rhs).trim
  }

  def &(rhs: UInt): Natural = End(digit & rhs)

  def ^(rhs: Natural): Natural = {
    def xor(lhs: Natural, rhs: Natural): Natural = lhs match {
      case End(ld) => rhs match {
        case End(rd) => End(ld ^ rd)
        case Digit(rd, rtail) => Digit(ld ^ rd, rtail)
      }
      case Digit(ld, ltail) => rhs match {
        case End(rd) => Digit(ld ^ rd, ltail)
        case Digit(rd, rtail) => Digit(ld ^ rd, ltail ^ rtail)
      }
    }
    xor(lhs, rhs).trim
  }

  def ^(rhs: UInt): Natural = lhs match {
    case End(ld) => End(ld ^ rhs)
    case Digit(ld, ltail) => Digit(ld ^ rhs, ltail)
  }
}

// TODO: maybe split apply into apply() and fromX()
// this way we can protect end-users from sign problems
object Natural extends NaturalInstances {
  private[math] final val denom = UInt(1000000000)

  implicit def naturalToBigInt(n: Natural): BigInt = n.toBigInt

  // required in big-endian order
  def apply(us: UInt*): Natural = {
    if (us.isEmpty) sys.error("invalid arguments")
    us.tail.foldLeft(End(us.head): Natural)((n, u) => Digit(u, n))
  }

  def apply(n: Long): Natural = if ((n & 0xffffffffL) == n)
    End(UInt(n.toInt))
  else
    Digit(UInt(n.toInt), End(UInt((n >> 32).toInt)))

  def apply(n: BigInt): Natural = if (n < 0)
    sys.error("negative numbers not allowd: %s" format n)
  else if (n < 0xffffffffL)
    End(UInt(n.toLong))
  else
    Digit(UInt((n & 0xffffffffL).toLong), apply(n >> 32))

  private val ten18 = Natural(1000000000000000000L)
  def apply(s: String): Natural = {
    def parse(sofar: Natural, s: String, m: Natural): Natural = if (s.length <= 18) {
      Natural(s.toLong) * m + sofar
    } else {
      val p = s.substring(s.length - 18, s.length)
      val r = s.substring(0, s.length - 18)
      parse(Natural(p.toLong) * m + sofar, r, m * ten18)
    }

    parse(Natural(0L), s, Natural(1L))
  }

  val zero: Natural = apply(0L)
  val one: Natural = apply(1L)

  @SerialVersionUID(0L)
  case class Digit(d: UInt, tl: Natural) extends Natural with Serializable {
    def digit: UInt = d
    def tail: Natural = tl

    def +(n: UInt): Natural = if (n == UInt(0)) {
      this
    } else {
      val t = d.toLong + n.toLong
      Digit(UInt(t), tail + UInt(t >> 32))
    }

    def -(n: UInt): Natural = if (n == UInt(0)) {
      this
    } else {
      val t = d.toLong - n.toLong
      Digit(UInt(t), tail - UInt(-(t >> 32)))
    }

    def *(n: UInt): Natural = if (n == UInt(0))
      End(n)
    else if (n == UInt(1))
      this
    else
      Natural(d.toLong * n.toLong) + Digit(UInt(0), tl * n)

    def /(n: UInt): Natural = (this /% n)._1

    def %(n: UInt): Natural = (this /% n)._2

    def /%(n: UInt): (Natural, Natural) = {
      @tailrec
      def recur(next: Natural, rem: UInt, sofar: Natural): (Natural, Natural) = {
        val t: ULong = ULong(rem.toLong << 32) + ULong(next.digit.toLong)
        val q: Long = (t / ULong(n.toLong)).toLong
        val r: Long = (t % ULong(n.toLong)).toLong

        next match {
          case Natural.End(d) => (Digit(UInt(q), sofar), End(UInt(r)))
          case Natural.Digit(d, tail) => recur(tail, UInt(r), Digit(UInt(q), sofar))
        }
      }

      if (n == UInt(0)) {
        sys.error("/ by zero")
      } else if (n == UInt(1)) {
        (this, Natural(UInt(0)))
      } else {
        reversed match {
          case Digit(d, tail) =>
            val q = d / n
            val r = d % n
            recur(tail, r, End(q))
          case _ =>
            sys.error("bug in reversed")
        }
      }
    }
  }

  @SerialVersionUID(0L)
  case class End(d: UInt) extends Natural with Serializable {
    def digit: UInt = d

    def +(n: UInt): Natural = if (n == UInt(0)) {
      this
    } else {
      val t = d.toLong + n.toLong
      if (t <= 0xffffffffL)
        End(UInt(t))
      else
        Digit(UInt(t), End(UInt(1)))
    }

    def -(n: UInt): Natural = if (n == UInt(0)) {
      this
    } else {
      val t = d.toLong - n.toLong
      if (t >= 0L)
        End(UInt(t.toInt))
      else
        sys.error("illegal subtraction: %s %s" format (this, n))
    }

    def *(n: UInt): Natural = if (n == UInt(0))
      End(n)
    else if (n == UInt(1))
      this
    else
      Natural(d.toLong * n.toLong)

    def /(n: UInt): Natural = if (n == UInt(0))
      sys.error("/ by zero")
    else
      End(d / n)

    def %(n: UInt): Natural = if (n == UInt(0))
      sys.error("/ by zero")
    else
      End(d % n)

    def /%(n: UInt): (Natural, Natural) = (this / n, this % n)
  }
}

trait NaturalInstances {
  implicit final val NaturalAlgebra = new NaturalAlgebra
}

private[math] trait NaturalIsRig extends Rig[Natural] {
  def one: Natural = Natural(1L)
  def plus(a:Natural, b:Natural): Natural = a + b
  override def pow(a:Natural, b:Int): Natural = {
    if (b < 0)
      throw new IllegalArgumentException("negative exponent: %s" format b)
    a pow UInt(b)
  }
  override def times(a:Natural, b:Natural): Natural = a * b
  def zero: Natural = Natural(0L)
}

private[math] trait NaturalOrder extends Order[Natural] {
  override def eqv(x: Natural, y: Natural) = x == y
  override def neqv(x: Natural, y: Natural) = x != y
  override def gt(x: Natural, y: Natural) = x > y
  override def gteqv(x: Natural, y: Natural) = x >= y
  override def lt(x: Natural, y: Natural) = x < y
  override def lteqv(x: Natural, y: Natural) = x <= y
  def compare(x: Natural, y: Natural) = x.compare(y)
}

private[math] trait NaturalIsSigned extends Signed[Natural] {
  def signum(a: Natural): Int = if (a == Natural.zero) 0 else 1
  def abs(a: Natural): Natural = a
}

private[math] trait NaturalIsReal extends IsIntegral[Natural]
with NaturalOrder with NaturalIsSigned {
  def toDouble(n: Natural): Double = n.toDouble
}

@SerialVersionUID(0L)
class NaturalAlgebra extends NaturalIsRig with NaturalIsReal with Serializable

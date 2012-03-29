package spire.math

import org.scalatest.FunSuite
import spire.math.fun._
import Implicits.{eqOps => _, _}
import java.math.BigInteger

object Const {
  val pos = 1
  val zero = 0
  val neg = -1
}

class SBigIntTest extends FunSuite {
  import Const._
  
  test("BigInt(0) same as BigInt.Zero") {
    assert(SBigInt(0) eq SBigInt.Zero)
  }
  
  test("BigInt(0) equal to BigInt.Zero") {
    assert(SBigInt(0) === SBigInt.Zero)
  }
  
  test("BigInt(1) equal to BigInt.One") {
    assert(SBigInt(1) === SBigInt.One)
  }
  
  test("create BigInt(42)") {
    val bi = SBigInt(42)
    assert(bi.signum === pos)
    assert(bi.arr === Array(42))
  }
    
  test("Compare toStrings of values from -100 to 100") {
    for (i <- -100 to 100) {
      assert(SBigInt(i).toString === (BigInteger.valueOf(i)).toString)
    }
  }
  
  test("Compare toStrings of values from Int.MaxValue.toLong -100 to Int.MaxValue.toLong + 100") {
    for (i <- Int.MaxValue.toLong -100 to Int.MaxValue.toLong + 100) {
      assert(SBigInt(i).toString === (BigInteger.valueOf(i)).toString)
    }
  }
  
  test("Compare toStrings of values from Int.MinValue.toLong -100 to Int.MinValue.toLong + 100") {
    for (i <- Int.MinValue.toLong -100 to Int.MinValue.toLong + 100) {
      assert(SBigInt(i).toString === (BigInteger.valueOf(i)).toString)
    }
  }
  
  test("Serialization") {
    import java.io._
    
    for (i <- -100 to 100) {
    
      val bi= SBigInt(i)
    
      val buffer = new ByteArrayOutputStream
      val out = new ObjectOutputStream(buffer)
      out writeObject bi
      out.close()
    
      val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
      val bi2 = in.readObject
      in.close()
    
      assert(bi === bi2)
    }
  }
}
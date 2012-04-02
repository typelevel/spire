package spire.math

import org.scalatest.FunSuite
import spire.math.fun._
import Implicits.{eqOps => _, _}
import java.math.BigInteger
import org.scalatest.PrivateMethodTester

object Const {
  val pos = 1
  val zero = 0
  val neg = -1
}

class SBigIntTest extends FunSuite with PrivateMethodTester {
  import Const._
  
  val interestingValues = 
    (-100L to 100L) ++
    (Int.MaxValue.toLong -100 to Int.MaxValue.toLong + 100) ++
    (Int.MinValue.toLong -100 to Int.MinValue.toLong + 100)
  
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
    
  test("Compare toStrings of interesting values") {
    for (i <- interestingValues) {
      assert(SBigInt(i).toString === BigInteger.valueOf(i).toString)
    }
  }
  
  test("Create BigInt from String") {
    for (i <- interestingValues take 201) {
      assert(SBigInt(i.toString).toString === new BigInteger(i.toString).toString)
    }
  }
  
  test("Serialization") {
    import java.io._
    
    for (i <- interestingValues) {
    
      val bi= SBigInt(i)
    
      val buffer = new ByteArrayOutputStream
      val out = new ObjectOutputStream(buffer)
      out writeObject bi
      
      if(i == 0 || i == 1)
        println(buffer.toByteArray.toList)
      
      out.close()
    
      val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
      val bi2 = in.readObject
      in.close()
    
      assert(bi === bi2)
    }
  }
  
  test("stripLeadingZeroes") {
    val input: Array[Array[Int]] = Array(Array(), Array(0), Array(0,0), Array(3,2,1,0), Array(0,1,0))
    val expected: Array[Array[Int]] = Array(Array(), Array(), Array(), Array(3,2,1), Array(0,1))
    
    val stripLeadingZeroes = PrivateMethod[Array[Int]]('stripLeadingZeroes)
    
    input.indices foreach { i =>
      val newArr = SBigInt invokePrivate stripLeadingZeroes(input(i))
      assert(newArr === expected(i))
    }
  }
}
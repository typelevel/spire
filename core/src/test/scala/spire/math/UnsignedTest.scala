package spire.math

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class ULongTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = ULong(0L)
  val one = ULong(1L)

  implicit val arbu: Arbitrary[ULong] = Arbitrary(arbitrary[Long].map(ULong(_)))

  property("n >= 0") {
    forAll { (n: ULong) => n >= zero should be === true }
  }

  property("a + b == b + a") {
    forAll { (a: ULong, b: ULong) => a + b should be === b + a }
  }

  property("a * b == b * a") {
    forAll { (a: ULong, b: ULong) => a * b should be === b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: ULong, b: ULong) => (a + b) - b should be === a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: ULong) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: ULong) =>
      n / one should be === n
      n % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: ULong, d: ULong) =>
      whenever (d != zero) {
        val q = n / d
        val r = n % d
        q * d + r should be === n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: ULong, d: ULong) =>
      whenever (d != zero) {
        n / d <= n should be === true
      }
    }
  }

  property("n % d < d") {
    forAll { (n: ULong, d: ULong) =>
      whenever (d != zero) {
        n % d < d should be === true
      }
    }
  }

  property("n + 1 > n") {
    forAll { (n: ULong) =>
      whenever (n != ULong.MaxValue) {
        n + one > n should be === true
      }
    }
  }

  property("a < b") {
    forAll { (a: ULong, b: ULong) => a < b should be === a.toBigInt < b.toBigInt }
  }

  property("a <= b") {
    forAll { (a: ULong, b: ULong) => a <= b should be === a.toBigInt <= b.toBigInt }
  }

  property("a > b") {
    forAll { (a: ULong, b: ULong) => a > b should be === a.toBigInt > b.toBigInt }
  }

  property("a >= b") {
    forAll { (a: ULong, b: ULong) => a >= b should be === a.toBigInt >= b.toBigInt }
  }
}

class UIntTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = UInt(0)
  val one = UInt(1)

  implicit val arbu: Arbitrary[UInt] = Arbitrary(arbitrary[Int].map(UInt(_)))

  property("n >= 0") {
    forAll { (n: UInt) =>
      n >= zero should be === true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UInt, b: UInt) => a + b should be === b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Int, b: Int) => a * b should be === b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UInt, b: UInt) => (a + b) - b should be === a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UInt) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: UInt) =>
      n / one should be === n
      n % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UInt, d: UInt) =>
      whenever (d != 0) {
        val q = n / d
        val r = n % d
        q * d + r should be === n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UInt, d: UInt) =>
      whenever (d != 0) { n / d <= n should be === true }
    }
  }

  property("n % d < d") {
    forAll { (n: UInt, d: UInt) =>
      whenever (d != 0) { n % d < d should be === true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UInt) =>
      whenever (n != UInt.MaxValue) { n + one > n should be === true }
    }
  }

  property("a < b") {
    forAll { (a: UInt, b: UInt) => a < b should be === a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UInt, b: UInt) => a <= b should be === a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UInt, b: UInt) => a > b should be === a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UInt, b: UInt) => a >= b should be === a.toLong >= b.toLong }
  }
}

class UShortTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = UShort(0)
  val one = UShort(1)

  implicit val arbu: Arbitrary[UShort] = Arbitrary(arbitrary[Short].map(UShort(_)))

  property("n >= 0") {
    forAll { (n: UShort) =>
      n >= zero should be === true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UShort, b: UShort) => a + b should be === b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Short, b: Short) => a * b should be === b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UShort, b: UShort) => (a + b) - b should be === a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UShort) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: UShort) =>
      n / one should be === n
      n % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UShort, d: UShort) =>
      whenever (d != 0) {
        val q = n / d
        val r = n % d
        q * d + r should be === n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UShort, d: UShort) =>
      whenever (d != 0) { n / d <= n should be === true }
    }
  }

  property("n % d < d") {
    forAll { (n: UShort, d: UShort) =>
      whenever (d != 0) { n % d < d should be === true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UShort) =>
      whenever (n != UShort.MaxValue) { n + one > n should be === true }
    }
  }

  property("a < b") {
    forAll { (a: UShort, b: UShort) => a < b should be === a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UShort, b: UShort) => a <= b should be === a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UShort, b: UShort) => a > b should be === a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UShort, b: UShort) => a >= b should be === a.toLong >= b.toLong }
  }
}

class UByteTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = UByte(0)
  val one = UByte(1)

  implicit val arbu: Arbitrary[UByte] = Arbitrary(arbitrary[Byte].map(UByte(_)))

  property("n >= 0") {
    forAll { (n: UByte) =>
      n >= zero should be === true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UByte, b: UByte) => a + b should be === b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Byte, b: Byte) => a * b should be === b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UByte, b: UByte) => (a + b) - b should be === a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UByte) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: UByte) =>
      n / one should be === n
      n % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UByte, d: UByte) =>
      whenever (d != 0) {
        val q = n / d
        val r = n % d
        q * d + r should be === n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UByte, d: UByte) =>
      whenever (d != 0) { n / d <= n should be === true }
    }
  }

  property("n % d < d") {
    forAll { (n: UByte, d: UByte) =>
      whenever (d != 0) { n % d < d should be === true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UByte) =>
      whenever (n != UByte.MaxValue) { n + one > n should be === true }
    }
  }

  property("a < b") {
    forAll { (a: UByte, b: UByte) => a < b should be === a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UByte, b: UByte) => a <= b should be === a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UByte, b: UByte) => a > b should be === a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UByte, b: UByte) => a >= b should be === a.toLong >= b.toLong }
  }
}

// class UShortTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

//   val zero = UShort(0)
//   val one = UShort(1)

//   property("n >= 0") {
//     forAll { (n: Char) =>
//       UShort(n) >= zero should be === true
//     }
//   }

//   property("a + b == b + a") {
//     forAll { (a: Char, b: Char) =>
//       UShort(a) + UShort(b) should be === UShort(b) + UShort(a)
//     }
//   }

//   property("a * b == b * a") {
//     forAll { (a: Char, b: Char) =>
//       UShort(a) * UShort(b) should be === UShort(b) * UShort(a)
//     }
//   }

//   property("(a + b) - b == a") {
//     forAll { (a: Char, b: Char) =>
//       (UShort(a) + UShort(b)) - UShort(b) should be === UShort(a)
//     }
//   }

//   property("n / 0 -> ArithmeticException") {
//     forAll { (n: Char) =>
//       val error = try {
//         UShort(n) / zero
//         false
//       } catch {
//         case _: ArithmeticException => true
//         case _: Exception => false
//       }
//       error should be === true
//     }
//   }

//   property("n / 1 == n") {
//     forAll { (n: Char) =>
//       UShort(n) / one should be === UShort(n)
//       UShort(n) % one should be === zero
//     }
//   }

//   property("(n / d) * d + (n % d) == n") {
//     forAll { (n: Char, d: Char) =>
//       whenever (d != 0) {
//         val q = UShort(n) / UShort(d)
//         val r = UShort(n) % UShort(d)
//         q * UShort(d) + r should be === UShort(n)
//       }
//     }
//   }

//   property("n / d <= n") {
//     forAll { (n: Char, d: Char) =>
//       whenever (d != 0) {
//         UShort(n) / UShort(d) <= UShort(n) should be === true
//       }
//     }
//   }

//   property("n % d < d") {
//     forAll { (n: Char, d: Char) =>
//       whenever (d != 0) {
//         UShort(n) % UShort(d) < UShort(d) should be === true
//       }
//     }
//   }

//   property("n + 1 > n") {
//     forAll { (n: Char) =>
//       whenever (n != -1) {
//         UShort(n) + UShort(1) > UShort(n) should be === true
//       }
//     }
//   }
// }

// class UByteTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

//   val zero = UByte(0)
//   val one = UByte(1)

//   property("UByte(-128) >> 1 == UByte(64)") {
//     (UByte(-128) >> 1) should be === UByte(64)
//   }

//   property("n >= 0") {
//     forAll { (n: Byte) =>
//       UByte(n) >= zero should be === true
//     }
//   }

//   property("a + b == b + a") {
//     forAll { (a: Byte, b: Byte) =>
//       UByte(a) + UByte(b) should be === UByte(b) + UByte(a)
//     }
//   }

//   property("a * b == b * a") {
//     forAll { (a: Byte, b: Byte) =>
//       UByte(a) * UByte(b) should be === UByte(b) * UByte(a)
//     }
//   }

//   property("(a + b) - b == a") {
//     forAll { (a: Byte, b: Byte) =>
//       (UByte(a) + UByte(b)) - UByte(b) should be === UByte(a)
//     }
//   }

//   property("n / 0 -> ArithmeticException") {
//     forAll { (n: Byte) =>
//       val error = try {
//         UByte(n) / zero
//         false
//       } catch {
//         case _: ArithmeticException => true
//         case _: Exception => false
//       }
//       error should be === true
//     }
//   }

//   property("n / 1 == n") {
//     forAll { (n: Byte) =>
//       UByte(n) / one should be === UByte(n)
//       UByte(n) % one should be === zero
//     }
//   }

//   property("(n / d) * d + (n % d) == n") {
//     forAll { (n: Byte, d: Byte) =>
//       whenever (d != 0) {
//         val q = UByte(n) / UByte(d)
//         val r = UByte(n) % UByte(d)
//         q * UByte(d) + r should be === UByte(n)
//       }
//     }
//   }

//   property("n / d <= n") {
//     forAll { (n: Byte, d: Byte) =>
//       whenever (d != 0) {
//         UByte(n) / UByte(d) <= UByte(n) should be === true
//       }
//     }
//   }

//   property("n % d < d") {
//     forAll { (n: Byte, d: Byte) =>
//       whenever (d != 0) {
//         UByte(n) % UByte(d) < UByte(d) should be === true
//       }
//     }
//   }

//   property("n + 1 > n") {
//     forAll { (n: Byte) =>
//       whenever (n != -1) {
//         UByte(n) + UByte(1) > UByte(n) should be === true
//       }
//     }
//   }

//   property("n << x == n << (x + 32 * y)") {
//     forAll { (n: Byte, x: Int, y: Int) =>
//       UByte(n) << x should be === UByte(n) << (x + 32 * y)
//     }
//   }

//   property("n >> x == n >> (x % 32)") {
//     forAll { (n: Byte, x: Int, y: Int) =>
//       UByte(n) >> x should be === UByte(n) >> (x % 32)
//     }
//   }

//   property("n >> x == n >>> x") {
//     forAll { (n: Byte, x: Byte) =>
//       UByte(n) >> x should be === UByte(n) >>> x
//     }
//   }
// }

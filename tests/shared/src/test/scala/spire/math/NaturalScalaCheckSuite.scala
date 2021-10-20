/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import org.scalacheck.Arbitrary._

import scala.util.Try
import org.scalacheck.Prop._

class NaturalScalaCheckSuite extends munit.ScalaCheckSuite {

  import spire.std.bigInt._
  import ArbitrarySupport._
  type N = NonNegative[BigInt]

  property("x + y") {
    forAll { (x: N, y: N) =>
      Natural(x.num) + Natural(y.num) == Natural(x.num + y.num)
    }
  }

  property("x - y") {
    forAll { (x: N, y: N) =>
      val z = Try(Natural(x.num) - Natural(y.num))
      if (x.num >= y.num) {
        z == Try(Natural(x.num - y.num))
      } else {
        z.isFailure == true
      }
    }
  }

  property("x * y") {
    forAll { (x: N, y: N) =>
      Natural(x.num) * Natural(y.num) == Natural(x.num * y.num)
    }
  }

  property("x / y") {
    forAll { (x: N, y: Positive[BigInt]) =>
      Natural(x.num) / Natural(y.num) == Natural(x.num / y.num)
    }
  }

  property("x % y") {
    forAll { (x: N, y: Positive[BigInt]) =>
      Natural(x.num) % Natural(y.num) == Natural(x.num % y.num)
    }
  }

  property("x /% y") {
    forAll { (x: N, y: Positive[BigInt]) =>
      (Natural(x.num) /% Natural(y.num)) == ((Natural(x.num / y.num), Natural(x.num % y.num)))
    }
  }

  property("x compare y") {
    forAll { (x: N, y: N) =>
      (Natural(x.num).compare(Natural(y.num))) == (x.num.compare(y.num))
    }
  }

  property("x.toString") {
    forAll { (x: N) =>
      Natural(x.num).toString == x.num.toString
    }
  }

  property("x.toBigInt") {
    forAll { (x: N) =>
      Natural(x.num).toBigInt == x.num
    }
  }

  property("x.toLong") {
    forAll { (x: N) =>
      Natural(x.num).toLong == x.num.toLong
    }
  }
}

package spire
package util

import org.scalatest.funsuite.AnyFunSuite
import spire.algebra.Eq

class OptCheck extends AnyFunSuite {

  def customToString[A](nullbox: Opt[A]): String = nullbox.toString
  def intToString(nullbox: Opt[Int]): String = nullbox.toString
  def strToString(nullbox: Opt[String]): String = nullbox.toString

  test("Opt.empty") {
    assert(Opt.empty[Int].isEmpty)
    assert(Opt.empty[String].isEmpty)
    assert(!Opt.empty[Int].nonEmpty)
    assert(!Opt.empty[String].nonEmpty)
    assert(!Opt.empty[Int].isDefined)
    assert(!Opt.empty[String].isDefined)

    assertResult("Opt.empty") { Opt.empty[Int].toString }
    assertResult("Opt.empty") { Opt.empty[String].toString }

    assertResult("Opt.empty") { customToString(Opt.empty[Int]) }
    assertResult("Opt.empty") { customToString(Opt.empty[String]) }
    assertResult("Opt.empty") { intToString(Opt.empty[Int]) }
    assertResult("Opt.empty") { strToString(Opt.empty[String]) }

    intercept[NoSuchElementException] { Opt.empty[Int].get }
    intercept[NoSuchElementException] { Opt.empty[String].get }
  }

  test("Opt Equality"){
    import spire.std.boolean._
    val eq = Eq[Opt[Boolean]]
    assert(eq.eqv(Opt(true) ,Opt(true)))
    assert(eq.eqv(Opt.empty ,Opt.empty))
    assert(eq.neqv(Opt.empty ,Opt(true)))
    assert(eq.neqv(Opt(true) ,Opt.empty))
  }

  test("Regression test for #896"){
    {
      import spire.std.boolean._
      val eq = Eq[Opt[Boolean]]
      assert(eq.neqv(Opt(false), Opt.empty))
    }
    {
      import spire.std.array._
      import spire.std.unit._
      val eq = Eq[Opt[Array[Unit]]]
      assertResult(false){
        eq.eqv(Opt(Array.ofDim(0)), Opt.empty)
      }
    }
  }

  test("Opt(value)") {
    assert(Opt(1).nonEmpty)
    assert(Opt("abc").nonEmpty)
    assert(Opt(1).isDefined)
    assert(Opt("abc").isDefined)
    assert(!Opt(1).isEmpty)
    assert(!Opt("abc").isEmpty)

    assertResult("Opt(1)") { Opt(1).toString }
    assertResult("Opt(abc)") { Opt("abc").toString }

    assertResult("Opt(1)") { customToString(Opt(1)) }
    assertResult("Opt(abc)") { customToString(Opt("abc")) }
    assertResult("Opt(1)") { intToString(Opt(1)) }
    assertResult("Opt(abc)") { strToString(Opt("abc")) }

    assertResult(1) { Opt(1).get }
    assertResult("abc") { Opt("abc").get }
  }

  test("for comprehension") {
    val a = Opt(33)
    val b = Opt(1999)
    val c = Opt(2)

    val d = a.filter(_ % 2 == 1)
    val e = b.map(_ + 999)

    assertResult(Opt(6029)) {
      for {
        q <- Opt(0)
        x <- d
        y <- e
        z <- c
      } yield q + x + y * z
    }
  }

  test("Name-based extractor") {
    Opt(2) match {
      case Opt(x) => // success
      case _ => fail()
    }
    Opt.empty[Int] match {
      case Opt(x) => fail()
      case _ => // success
    }
  }

  test("Opt.filter") {
    def isEven(i: Int): Boolean = (i % 2 == 0)
    assert(Opt(1).filter(isEven).isEmpty)
    assertResult(2)(Opt(2).filter(isEven).get)
    assert(Opt.empty[Int].filter(_ % 2 == 0).isEmpty)
  }

  def parseInt(str: String): Opt[Int] = try {
    Opt(java.lang.Integer.parseInt(str))
  } catch {
    case e: NumberFormatException => Opt.empty[Int]
  }

  test("Opt.map") {
    assertResult(Opt(2))(Opt(1).map(_ * 2))
    assertResult(Opt("2"))(Opt(2).map(_.toString))
    assertResult(Opt(2))(parseInt("2"))
    assertResult(Opt.empty[Int])(parseInt("abc"))
  }

  test("Opt.flatMap") {
    assertResult(Opt(2))(Opt("2").flatMap(parseInt))
    assertResult(Opt.empty[Int])(Opt("abc").flatMap(parseInt))
    assertResult(Opt.empty[Int])(Opt.empty[String].flatMap(parseInt))
  }

  test("Opt.fold") {
    assertResult(2)(Opt(1).fold(0)(_ * 2))
    assertResult(0)(Opt.empty[Int].fold(0)(_ * 2))
    assertResult("abcabc")(Opt("abc").fold("")(_ * 2))
    assertResult("")(Opt.empty[String].fold("")(_ * 2))
  }

  test("Opt.getOrElse") {
    assertResult(2)(Opt(2).getOrElse(0))
    assertResult(0)(Opt.empty[Int].getOrElse(0))
    assertResult("abc")(Opt("abc").getOrElse(""))
    assertResult("")(Opt.empty[String].getOrElse(""))
  }

  test("Opt.getOrElseFast") {
    assertResult(2)(Opt(2).getOrElseFast(0))
    assertResult(0)(Opt.empty[Int].getOrElseFast(0))
    assertResult("abc")(Opt("abc").getOrElseFast(""))
    assertResult("")(Opt.empty[String].getOrElseFast(""))
  }

  test("Opt.toOption") {
    assertResult(Some(2))(Opt(2).toOption)
    assertResult(None)(Opt.empty[Int].toOption)
    assertResult(Some("abc"))(Opt("abc").toOption)
    assertResult(None)(Opt.empty[String].toOption)
  }
}

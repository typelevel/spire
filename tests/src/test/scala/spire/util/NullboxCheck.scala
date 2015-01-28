package spire.util

import org.scalatest.FunSuite

class NullboxCheck extends FunSuite {

  def customToString[A](nullbox: Nullbox[A]): String = nullbox.toString
  def intToString(nullbox: Nullbox[Int]): String = nullbox.toString
  def strToString(nullbox: Nullbox[String]): String = nullbox.toString

  test("Nullbox.empty") {
    assert(Nullbox.empty[Int].isEmpty)
    assert(Nullbox.empty[String].isEmpty)
    assert(!Nullbox.empty[Int].nonEmpty)
    assert(!Nullbox.empty[String].nonEmpty)
    assert(!Nullbox.empty[Int].isDefined)
    assert(!Nullbox.empty[String].isDefined)

    assertResult("Nullbox.empty") { Nullbox.empty[Int].toString }
    assertResult("Nullbox.empty") { Nullbox.empty[String].toString }

    assertResult("Nullbox.empty") { customToString(Nullbox.empty[Int]) }
    assertResult("Nullbox.empty") { customToString(Nullbox.empty[String]) }
    assertResult("Nullbox.empty") { intToString(Nullbox.empty[Int]) }
    assertResult("Nullbox.empty") { strToString(Nullbox.empty[String]) }

    intercept[NoSuchElementException] { Nullbox.empty[Int].get }
    intercept[NoSuchElementException] { Nullbox.empty[String].get }
  }

  test("Nullbox(value)") {
    assert(Nullbox(1).nonEmpty)
    assert(Nullbox("abc").nonEmpty)
    assert(Nullbox(1).isDefined)
    assert(Nullbox("abc").isDefined)
    assert(!Nullbox(1).isEmpty)
    assert(!Nullbox("abc").isEmpty)

    assertResult("Nullbox(1)") { Nullbox(1).toString }
    assertResult("Nullbox(abc)") { Nullbox("abc").toString }

    assertResult("Nullbox(1)") { customToString(Nullbox(1)) }
    assertResult("Nullbox(abc)") { customToString(Nullbox("abc")) }
    assertResult("Nullbox(1)") { intToString(Nullbox(1)) }
    assertResult("Nullbox(abc)") { strToString(Nullbox("abc")) }

    assertResult(1) { Nullbox(1).get }
    assertResult("abc") { Nullbox("abc").get }
  }

  test("for comprehension") {
    val a = Nullbox(33)
    val b = Nullbox(1999)
    val c = Nullbox(2)
    
    val d = a.filter(_ % 2 == 1)
    val e = b.map(_ + 999)
    
    assertResult(Nullbox(6029)) {
      for {
        q <- Nullbox(0)
        x <- d
        y <- e
        z <- c
      } yield q + x + y * z
    }
  }

  test("Name-based extractor") {
    Nullbox(2) match {
      case Nullbox(x) => // success
      case _ => fail()
    }
    Nullbox.empty[Int] match {
      case Nullbox(x) => fail()
      case _ => // success
    }
  }

  test("Nullbox.filter") {
    def isEven(i: Int): Boolean = (i % 2 == 0)
    assert(Nullbox(1).filter(isEven).isEmpty)
    assertResult(2)(Nullbox(2).filter(isEven).get)
    assert(Nullbox.empty[Int].filter(_ % 2 == 0).isEmpty)
  }

  def parseInt(str: String): Nullbox[Int] = try {
    Nullbox(java.lang.Integer.parseInt(str))
  } catch {
    case e: NumberFormatException => Nullbox.empty[Int]
  }

  test("Nullbox.map") {
    assertResult(Nullbox(2))(Nullbox(1).map(_ * 2))
    assertResult(Nullbox("2"))(Nullbox(2).map(_.toString))
    assertResult(Nullbox(2))(parseInt("2"))
    assertResult(Nullbox.empty[Int])(parseInt("abc"))
  }

  test("Nullbox.flatMap") {
    assertResult(Nullbox(2))(Nullbox("2").flatMap(parseInt))
    assertResult(Nullbox.empty[Int])(Nullbox("abc").flatMap(parseInt))
    assertResult(Nullbox.empty[Int])(Nullbox.empty[String].flatMap(parseInt))
  }

  test("Nullbox.fold") {
    assertResult(2)(Nullbox(1).fold(0)(_ * 2))
    assertResult(0)(Nullbox.empty[Int].fold(0)(_ * 2))
    assertResult("abcabc")(Nullbox("abc").fold("")(_ * 2))
    assertResult("")(Nullbox.empty[String].fold("")(_ * 2))
  }

  test("Nullbox.getOrElse") {
    assertResult(2)(Nullbox(2).getOrElse(0))
    assertResult(0)(Nullbox.empty[Int].getOrElse(0))
    assertResult("abc")(Nullbox("abc").getOrElse(""))
    assertResult("")(Nullbox.empty[String].getOrElse(""))
  }

  test("Nullbox.getOrElseFast") {
    assertResult(2)(Nullbox(2).getOrElseFast(0))
    assertResult(0)(Nullbox.empty[Int].getOrElseFast(0))
    assertResult("abc")(Nullbox("abc").getOrElseFast(""))
    assertResult("")(Nullbox.empty[String].getOrElseFast(""))
  }

  test("Nullbox.toOption") {
    assertResult(Some(2))(Nullbox(2).toOption)
    assertResult(None)(Nullbox.empty[Int].toOption)
    assertResult(Some("abc"))(Nullbox("abc").toOption)
    assertResult(None)(Nullbox.empty[String].toOption)
  }
}

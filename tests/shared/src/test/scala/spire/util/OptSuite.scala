package spire
package util

import spire.algebra.Eq

class OptSuite extends munit.FunSuite {

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

    assertEquals("Opt.empty", Opt.empty[Int].toString)
    assertEquals("Opt.empty", Opt.empty[String].toString)

    assertEquals("Opt.empty", customToString(Opt.empty[Int]))
    assertEquals("Opt.empty", customToString(Opt.empty[String]))
    assertEquals("Opt.empty", intToString(Opt.empty[Int]))
    assertEquals("Opt.empty", strToString(Opt.empty[String]))

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
      assert(!eq.eqv(Opt(Array.ofDim(0)), Opt.empty))
    }
  }

  test("Opt(value)") {
    assert(Opt(1).nonEmpty)
    assert(Opt("abc").nonEmpty)
    assert(Opt(1).isDefined)
    assert(Opt("abc").isDefined)
    assert(!Opt(1).isEmpty)
    assert(!Opt("abc").isEmpty)

    assertEquals("Opt(1)", Opt(1).toString)
    assertEquals("Opt(abc)", Opt("abc").toString)

    assertEquals("Opt(1)", customToString(Opt(1)))
    assertEquals("Opt(abc)", customToString(Opt("abc")))
    assertEquals("Opt(1)", intToString(Opt(1)))
    assertEquals("Opt(abc)", strToString(Opt("abc")))

    assertEquals(1, Opt(1).get)
    assertEquals("abc", Opt("abc").get)
  }

  test("for comprehension") {
    val a = Opt(33)
    val b = Opt(1999)
    val c = Opt(2)

    val d = a.filter(_ % 2 == 1)
    val e = b.map(_ + 999)

    assertEquals(Opt(6029), {
      for {
        q <- Opt(0)
        x <- d
        y <- e
        z <- c
      } yield q + x + y * z
    })
  }

  test("Name-based extractor") {
    assert(Opt(2) match {
      case Opt(x) => true
      case _ => false
    })
    assert(Opt.empty[Int] match {
      case Opt(x) => false
      case _ => true
    })
  }

  test("Opt.filter") {
    def isEven(i: Int): Boolean = (i % 2 == 0)
    assert(Opt(1).filter(isEven).isEmpty)
    assertEquals(2, Opt(2).filter(isEven).get)
    assert(Opt.empty[Int].filter(_ % 2 == 0).isEmpty)
  }

  def parseInt(str: String): Opt[Int] = try {
    Opt(java.lang.Integer.parseInt(str))
  } catch {
    case e: NumberFormatException => Opt.empty[Int]
  }

  test("Opt.map") {
    assertEquals(Opt(2), Opt(1).map(_ * 2))
    assertEquals(Opt("2"), Opt(2).map(_.toString))
    assertEquals(Opt(2), parseInt("2"))
    assertEquals(Opt.empty[Int], parseInt("abc"))
  }

  test("Opt.flatMap") {
    assertEquals(Opt(2), Opt("2").flatMap(parseInt))
    assertEquals(Opt.empty[Int], Opt("abc").flatMap(parseInt))
    assertEquals(Opt.empty[Int], Opt.empty[String].flatMap(parseInt))
  }

  test("Opt.fold") {
    assertEquals(2, Opt(1).fold(0)(_ * 2))
    assertEquals(0, Opt.empty[Int].fold(0)(_ * 2))
    assertEquals("abcabc", Opt("abc").fold("")(_ * 2))
    assertEquals("", Opt.empty[String].fold("")(_ * 2))
  }

  test("Opt.getOrElse") {
    assertEquals(2, Opt(2).getOrElse(0))
    assertEquals(0, Opt.empty[Int].getOrElse(0))
    assertEquals("abc", Opt("abc").getOrElse(""))
    assertEquals("", Opt.empty[String].getOrElse(""))
  }

  test("Opt.getOrElseFast") {
    assertEquals(2, Opt(2).getOrElseFast(0))
    assertEquals(0, Opt.empty[Int].getOrElseFast(0))
    assertEquals("abc", Opt("abc").getOrElseFast(""))
    assertEquals("", Opt.empty[String].getOrElseFast(""))
  }

  test("Opt.toOption") {
    assertEquals(Some(2): Option[Int], Opt(2).toOption)
    assertEquals(None: Option[Int], Opt.empty[Int].toOption)
    assertEquals(Some("abc"): Option[String], Opt("abc").toOption)
    assertEquals(None: Option[String], Opt.empty[String].toOption)
  }
}

package spire
package tests

import org.scalatest.Matchers
import org.scalatest._
import prop._

trait SpireProperties extends PropSpec with Matchers with PropertyChecks {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

trait SpireTests extends FunSuite with Matchers {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

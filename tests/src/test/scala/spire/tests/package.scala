package spire
package tests

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

trait SpireProperties extends AnyPropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

trait SpireTests extends AnyFunSuite with Matchers {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

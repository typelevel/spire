package spire
package tests

import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

trait SpireProperties extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

trait SpireTests extends AnyFunSuite with Matchers {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

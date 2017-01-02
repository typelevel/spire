package spire
package syntax

import shapeless.test.illTyped
import spire.tests.SpireProperties
import spire.implicits._

class StrictEqTest extends SpireProperties {

  property("negation") {
    assert(1 === 1)
    assert(!(1 =!= 1))
    assert(!(1 === 2))
    assert(1 =!= 2)
  }

  property("syntax") {
    illTyped("1 === 1L")
    illTyped("1L === 1")
    illTyped("""1 === "x"""")
  }
}

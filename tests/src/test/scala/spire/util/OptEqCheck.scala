package spire
package util

import org.scalatest.FunSuite
import spire.algebra.Eq

class OptEqCheck extends FunSuite {

  test("Opt Equality"){
    import spire.std.opt._
    import spire.std.boolean._
    val eq = Eq[Opt[Boolean]]
    assert(eq.eqv(Opt(true), Opt(true)))
    assert(eq.eqv(Opt.empty, Opt.empty))
    assert(eq.neqv(Opt.empty, Opt(true)))
    assert(eq.neqv(Opt(true), Opt.empty))
  }

}

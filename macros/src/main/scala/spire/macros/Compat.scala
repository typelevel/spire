package spire.macros

import scala.reflect.macros.Context

object Compat {

  type OldContext = Context

  def freshTermName[C <: OldContext](c: C)(s: String) =
    c.universe.newTermName(c.fresh(s))

  def termName[C <: OldContext](c: C)(s: String) =
    c.universe.newTermName(s)

  def typeCheck[C <: OldContext](c: C)(t: c.Tree) =
    c.typeCheck(t)

  def resetLocalAttrs[C <: OldContext](c: C)(t: c.Tree) =
    c.resetLocalAttrs(t)
}

package spire.macros

object compat {

  type Context = scala.reflect.macros.whitebox.Context

  def freshTermName[C <: Context](c: C)(s: String) =
    c.universe.TermName(c.freshName(s))

  def termName[C <: Context](c: C)(s: String) =
    c.universe.TermName(s)

  def typeCheck[C <: Context](c: C)(t: c.Tree) =
    c.typecheck(t)

  def resetLocalAttrs[C <: Context](c: C)(t: c.Tree) =
    c.untypecheck(t)
}

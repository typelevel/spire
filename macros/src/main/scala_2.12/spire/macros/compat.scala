package spire
package macros

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

  def setOrig[C <: Context](c: C)(tt: c.universe.TypeTree, t: c.Tree) =
    c.universe.internal.setOriginal(tt, t)

  def predef[C <: Context](c: C): c.Tree = {
    import c.universe._
    q"scala.Predef"
  }

}

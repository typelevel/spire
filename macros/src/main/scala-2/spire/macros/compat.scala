/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

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

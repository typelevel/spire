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
package syntax

import spire.implicits._

class StrictEqSuite extends munit.FunSuite {

  test("negation") {
    assert(1 === 1)
    assert(!(1 =!= 1))
    assert(!(1 === 2))
    assert(1 =!= 2)
  }

  test("syntax") {
    compileErrors("1 === 1L")
  }

  test("syntax2") {
    compileErrors("1L === 1")
  }

  test("syntax3") {
    compileErrors("""1 === "x"""")
  }
}

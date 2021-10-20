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
package example

/**
 * This is a Ziggurat generator of MATLAB files for data analysis via histogram and Distribution Fitting App (dfittool).
 *
 * <p><b>Reference: </b> George Marsaglia, Wai Wan Tsang: "The Ziggurat Method for Generating Random Variables"
 * <i>Journal of Statistical Software,</i> Vol. 5, Issue 8, October 2000.
 *
 * @see
 *   <a href="http://www.jstatsoft.org/v05/i08">Ziggurat Paper</a>
 * @see
 *   <a href="http://en.wikipedia.org/wiki/Ziggurat_algorithm">Ziggurat algorithm @ Wikipedia</a>
 * @author
 *   <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */
object ZigguratGenerator {

  def main(a: Array[String]) = {

    val g: spire.random.Generator = spire.random.rng.Well512a.fromTime()
    def samples = 200000
    def generate(d: spire.random.Generator => Double, n: Int) = Array.tabulate(n)(x => d(g))

    val rnor = generate(spire.random.Ziggurat.rnor, samples)
    val rexp = generate(spire.random.Ziggurat.rexp, samples)

    val fnor = new java.io.PrintStream(new java.io.FileOutputStream("zigguratrnor.m"))
    val fexp = new java.io.PrintStream(new java.io.FileOutputStream("zigguratrexp.m"))

    fnor.print("rnor = [")
    for (i <- rnor) { fnor.print(i); fnor.print(", ") }
    fnor.println("]")
    fnor.println("x = -5:0.05:5")
    fnor.println("hist(rnor,x)")
    fnor.println("dfittool(rnor)")
    fnor.close()

    fexp.print("rexp = [")
    for (i <- rexp) { fexp.print(i); fexp.print(", ") }
    fexp.println("]")
    fexp.println("x = -1:0.05:10")
    fexp.println("hist(rexp,x)")
    fexp.println("dfittool(rexp)")
    fexp.close()
  }
}

package spire
package benchmark.jmh

import spire.benchmark.FixtureSupport

trait StateSupport extends FixtureSupport {
  //TODO Make sizes configurable
  //val size = 10 * 1000
  //val size = 100 * 1000
  val size = 200 * 1000
  //val size = 1 * 1000 * 1000
  //val size = 4 * 1000 * 1000
  //val size = 20 * 1000 * 1000
}

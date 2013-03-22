package spire.algebra

import org.scalacheck.Properties

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait LawChecker extends FunSuite with Checkers {

  def checkAll(name: String, props: Properties) {
    for ((id, prop) ← props.properties)
      test(name + "." + id) {
        check(prop)
      }
  }

}

// vim: expandtab:ts=2:sw=2

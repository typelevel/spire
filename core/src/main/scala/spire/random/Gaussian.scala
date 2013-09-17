package spire.random

import scala.{specialized => spec}

import spire.math._

trait Gaussian[@spec A] {
  def apply(mean: A, stdDev: A): A
}

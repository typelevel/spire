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

import spire.algebra._
import spire.implicits._

import org.scalacheck.{Prop, Properties}

import org.typelevel.discipline.Predicate

package object laws {

  type DeMorganLaws[A] = _root_.algebra.laws.DeMorganLaws[A]
  val DeMorganLaws = _root_.algebra.laws.DeMorganLaws

  implicit def PredicateFromMonoid[A: Eq](implicit A: AdditiveMonoid[A]): Predicate[A] = new Predicate[A] {
    def apply(a: A) = a =!= A.zero
  }

  def propertiesToProp(properties: Properties) = Prop.all(properties.properties.map(_._2).toSeq: _*)

}

// vim: expandtab:ts=2:sw=2

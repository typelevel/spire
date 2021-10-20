/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
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
package algebra

package object lattice {
  type Logic[A] = _root_.algebra.lattice.Logic[A]
  val Logic = _root_.algebra.lattice.Logic

  type Heyting[A] = _root_.algebra.lattice.Heyting[A]
  val Heyting = _root_.algebra.lattice.Heyting

  type DeMorgan[A] = _root_.algebra.lattice.DeMorgan[A]
  val DeMorgan = _root_.algebra.lattice.DeMorgan

  type Lattice[A] = _root_.algebra.lattice.Lattice[A]
  val Lattice = _root_.algebra.lattice.Lattice

  type MeetSemilattice[A] = _root_.algebra.lattice.MeetSemilattice[A]
  val MeetSemilattice = _root_.algebra.lattice.MeetSemilattice

  type JoinSemilattice[A] = _root_.algebra.lattice.JoinSemilattice[A]
  val JoinSemilattice = _root_.algebra.lattice.JoinSemilattice

  type BoundedLattice[A] = _root_.algebra.lattice.BoundedLattice[A]
  val BoundedLattice = _root_.algebra.lattice.BoundedLattice

  type BoundedJoinSemilattice[A] = _root_.algebra.lattice.BoundedJoinSemilattice[A]
  val BoundedJoinSemilattice = _root_.algebra.lattice.BoundedJoinSemilattice

  type BoundedMeetSemilattice[A] = _root_.algebra.lattice.BoundedMeetSemilattice[A]
  val BoundedMeetSemilattice = _root_.algebra.lattice.BoundedMeetSemilattice

  type MinMaxLattice[A] = _root_.algebra.lattice.MinMaxLattice[A]
  // val MinMaxLattice = _root_.algebra.lattice.MinMaxLattice
}

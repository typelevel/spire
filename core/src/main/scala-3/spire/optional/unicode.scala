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
package optional
package unicode

import spire.algebra._
import spire.algebra.lattice._
import spire.math._

type ℍ = Quaternion[Real]
type ℂ = Complex[Real]
type ℝ = Real
type ℚ = Rational
type ℤ = SafeLong
type ℕ = Natural

val ℝ = Real
val ℚ = Rational
val ℤ = SafeLong
val ℕ = Natural

val ⅇ = Real.e
val π = Real.pi
val φ = Real.phi
val ⅈ = Complex.i[Real]
val ⅉ = Quaternion.j[Real]

def ⊤[A](using ev: Heyting[A]): A = ev.one
def ⊥[A](using ev: Heyting[A]): A = ev.zero
def ¬[A](a: A)(using ev: Heyting[A]): A = ev.complement(a)
def √[A](a: A)(using ev: NRoot[A]): A = ev.sqrt(a)
def ∛[A](a: A)(using ev: NRoot[A]): A = ev.nroot(a, 3)
def ∜[A](a: A)(using ev: NRoot[A]): A = ev.nroot(a, 4)

def Σ[A](as: Iterable[A])(using ev: AdditiveMonoid[A]): A =
  as.foldLeft(ev.zero)(ev.plus)

def Π[A](as: Iterable[A])(using ev: MultiplicativeMonoid[A]): A =
  as.foldLeft(ev.one)(ev.times)

extension [A](lhs: A)(using ev: MultiplicativeSemigroup[A]) def ∙(rhs: A): A = ev.times(lhs, rhs)

extension [A](lhs: A)(using ev: Eq[A])
  def ≡(rhs: A): Boolean = ev.eqv(lhs, rhs)
  def ≠(rhs: A): Boolean = ev.neqv(lhs, rhs)

extension [A](lhs: A)(using ev: PartialOrder[A])
  def ≤(rhs: A): Boolean = ev.lteqv(lhs, rhs)
  def ≥(rhs: A): Boolean = ev.gteqv(lhs, rhs)

extension [A](lhs: A)(using ev: MeetSemilattice[A]) def ∧(rhs: A): A = ev.meet(lhs, rhs)

extension [A](lhs: A)(using ev: JoinSemilattice[A]) def ∨(rhs: A): A = ev.join(lhs, rhs)

extension [A](lhs: A)(using ev: Heyting[A]) def ⊃(rhs: A): A = ev.imp(lhs, rhs)

extension [A](lhs: A)(using ev: Bool[A])
  def ⊻(rhs: A): A = ev.xor(lhs, rhs)
  def ⊼(rhs: A): A = ev.nand(lhs, rhs)
  def ⊽(rhs: A): A = ev.nor(lhs, rhs)

extension [A](lhs: Set[A])
  def ∋(a: A): Boolean = lhs(a)
  def ∌(a: A): Boolean = !lhs(a)

  def ∈:(a: A): Boolean = lhs(a)
  def ∉:(a: A): Boolean = !lhs(a)

  def ∩(rhs: Set[A]): Set[A] = lhs & rhs
  def ∪(rhs: Set[A]): Set[A] = lhs | rhs
  def \(rhs: Set[A]): Set[A] = lhs -- rhs

  def ⊂(rhs: Set[A]): Boolean = lhs.size < rhs.size && lhs.forall(rhs)
  def ⊃(rhs: Set[A]): Boolean = lhs.size > rhs.size && rhs.forall(lhs)

  def ⊆(rhs: Set[A]): Boolean = lhs.size <= rhs.size && lhs.forall(rhs)
  def ⊇(rhs: Set[A]): Boolean = lhs.size >= rhs.size && rhs.forall(lhs)

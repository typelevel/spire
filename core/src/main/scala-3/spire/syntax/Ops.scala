package spire
package syntax

import spire.algebra._
import spire.algebra.partial._

final class GroupoidCommonOps[A](lhs: A)(implicit ev: Groupoid[A]):
  def inverse: A = ev.inverse(lhs)
  def isId(implicit ev1: Eq[A]): Boolean = ev.isId(lhs)(ev1)

final class LeftModuleOps[V](x: V) extends AnyVal:
  def *:[F](lhs: F)(implicit ev: LeftModule[V, F]): V = ev.timesl(lhs, x)
  def *:[F](lhs: Int)(implicit ev: LeftModule[V, F], F: Ring[F]): V = ev.timesl(F.fromInt(lhs), x)

final class RightModuleOps[V](x: V) extends AnyVal:
  def :*[F](rhs: F)(implicit ev: RightModule[V, F]): V = ev.timesr(x, rhs)
  def :*[F](rhs: Int)(implicit ev: RightModule[V, F], F: Ring[F]): V = ev.timesr(x, F.fromInt(rhs))

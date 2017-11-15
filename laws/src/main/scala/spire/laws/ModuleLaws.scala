package spire.laws

import spire.algebra._

import org.typelevel.discipline.Laws

trait LeftModuleLaws[V, R] extends AdditiveAbGroupLaws[V] {
  implicit def R: Ring[R]
  override implicit def S: LeftModule[V, R]

  def leftScalarDistributes(r: R, v: V, w: V): IsEq[V] =
    S.timesl(r, S.plus(v, w)) <=> S.plus(S.timesl(r, v), S.timesl(r, w))

  def leftVectorDistributes(r: R, s: R, v: V): IsEq[V] =
    S.timesl(R.plus(r, s), v) <=> S.plus(S.timesl(r, v), S.timesl(s, v))

  def leftAssociativeScalar(r: R, s: R, v: V): IsEq[V] =
    S.timesl(R.times(r, s), v) <=> S.timesl(r, S.timesl(s, v))

  def leftIdentity(v: V): IsEq[V] =
    S.timesl(R.one, v) <=> v
}

object LeftModuleLaws {
  def apply[V, R:Ring](implicit ev: LeftModule[V, R]): LeftModuleLaws[V, R] =
    new LeftModuleLaws[V, R] { def R = implicitly; def S = implicitly }
}

trait RightModuleLaws[V, R] extends AdditiveAbGroupLaws[V] {
  implicit def R: Ring[R]
  override implicit def S: RightModule[V, R]

  def rightScalarDistributes(v: V, w: V, r: R): IsEq[V] =
    S.timesr(S.plus(v, w), r) <=> S.plus(S.timesr(v, r), S.timesr(w, r))

  def rightVectorDistributes(r: R, s: R, v: V): IsEq[V] =
    S.timesr(v, R.plus(r, s)) <=> S.plus(S.timesr(v, r), S.timesr(v, s))

  def rightAssociativeScalar(r: R, s: R, v: V): IsEq[V] =
    S.timesr(v, R.times(r, s)) <=> S.timesr(S.timesr(v, r), s)

  def rightIdentity(v: V): IsEq[V] =
    S.timesr(v, R.one) <=> v
}

object RightModuleLaws {
  def apply[V, R:Ring](implicit ev: RightModule[V, R]): RightModuleLaws[V, R] =
    new RightModuleLaws[V, R] { def R = implicitly; def S = implicitly }
}

trait CModuleLaws[V, R] extends LeftModuleLaws[V, R] with RightModuleLaws[V, R] {
  override implicit def R: CRing[R]
  override implicit def S: CModule[V, R]
  
  def leftRightMultiplicationCompatible(r: R, v: V, s: R): IsEq[V] =
    S.timesl(r, S.timesr(v, s)) <=> S.timesr(S.timesl(r, v), s)
}

object CModuleLaws {
  def apply[V, R:CRing](implicit ev: CModule[V, R]): CModuleLaws[V, R] =
    new CModuleLaws[V, R] { def R = implicitly; def S = implicitly }
}

trait VectorSpaceLaws[V, R] extends CModuleLaws[V, R] {
  override implicit def R: Field[R]
  override implicit def S: VectorSpace[V, R]
}

trait MetricSpaceLaws[V, R] extends Laws {
  implicit def R: AdditiveMonoid[R]
  implicit def S: MetricSpace[V, R]

  def identity(x: V): IsEq[R] = S.distance(x, x) <=> R.zero

  def zeroNonZero(x: V, y: V, eqV: Eq[V], eqR: Eq[R]): IsEq[Boolean] =
    eqR.eqv(S.distance(x, y), R.zero) <=> eqV.eqv(x, y)

  def symmetric(x: V, y: V): IsEq[R] = S.distance(x, y) <=> S.distance(y, x)

  def triangleInequality(x: V, y: V, z: V, orderR: Order[R]): IsEq[Boolean] =
    orderR.lteqv(S.distance(x, z), R.plus(S.distance(x, y), S.distance(y, z))) <=> true

  def nonNegative(x: V, y: V, signedR: Signed[R]): IsEq[Boolean] =
    (signedR.sign(S.distance(x, y)) != Sign.Negative) <=> true

}

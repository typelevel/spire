package spire.laws

import spire.algebra.CRig

trait CRigLaws[A] extends RigLaws[A] with CSemiringLaws[A] with MultiplicativeCMonoidLaws[A] {
  override implicit def S: CRig[A]
}

object CRigLaws {
  def apply[A](implicit ev: CRig[A]): CRigLaws[A] =
    new CRigLaws[A] { def S: CRig[A] = ev }
}

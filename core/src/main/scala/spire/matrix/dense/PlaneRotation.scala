package spire.matrix.dense

import scala.math._

/**
 * A plane rotation
 *
 * <pre>
 *      [ cs  -sn ]
 *      [ sn   cs ]
 * </pre>
 * where cs and sn are respectively the cosine and sine of the rotation angle
 */
final case class PlaneRotation(val cs:Double, val sn:Double) {

  def angle: Double = atan2(sn, cs)

  override def toString = {
    val deg = toDegrees(angle)
    s"$deg° plane rotation\n[ $cs ${-sn} ]\n[ $sn  $cs ]"
  }

  /** Vector of dimension 2 */
  type Vector2d  = (Double, Double)

  /** Matrix of dimension 2 x 2 laid out in row-major format */
  type Matrix2x2 = (Double, Double,
                    Double, Double)

  /** Transform the vector (x,y) */
  def apply(v:Vector2d): Vector2d = {
    val (x, y) = v
    (cs*x - sn*y, sn*x + cs*y)
  }

  /**
   * The product R M where R is this rotation
   */
  def applyOnLeft(m: Matrix2x2): Matrix2x2 = {
    val (a, b,
         c, d) = m
    val (aa, cc) = this((a, c))
    val (bb, dd) = this((b, d))
    (aa, bb,
     cc, dd)
  }

  /**
   * The product M R^T^ where R is this rotation
   */
  def applyTransposeOnRight(m: Matrix2x2): Matrix2x2 = {
    val (a, b,
         c, d) = m
    val (aa, bb) = this((a, b))
    val (cc, dd) = this((c, d))
    (aa, bb,
     cc, dd)
  }

  /**
   * Similarity transform R M R^T^ where R is this rotation
   */
  def applySimilarity(m: Matrix2x2): Matrix2x2 =
    applyOnLeft(applyTransposeOnRight(m))

  def inverse = PlaneRotation(cs, -sn)

  def *(r:PlaneRotation): PlaneRotation =
    PlaneRotation(cs*r.cs - sn*r.sn, sn*r.cs + cs*r.sn)

  def timesPositiveQuarterRotation = PlaneRotation(-sn, cs)
}

object PlaneRotation {

  def identity = PlaneRotation(1, 0)

  def positiveQuarterRotation = PlaneRotation(0, 1)
}

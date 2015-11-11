package spire
package laws

/**
 * Represents a permutation encoded as a map from preimages to images, including
 * only pairs that are moved by the permutation (so the identity is Map.empty).
 */
case class Perm(map: Map[Int, Int])

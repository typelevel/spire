package spire.algebra

/**
 * A (left) group action of `G` on `P` is simply the implementation of a method
 * `actl(g, p)` s.t. `actl(g |+| h, p) === actl(g, actl(h, p))` for all
 * `g`, `h` in `G` and `p` in `P` and `actl(scalar.id, p) === p`.
 */
trait GroupAction[P, G] {
  def scalar: Group[G]

  def actl(g: G, p: P): P
  def actr(p: P, g: G): P = actl(g, p)
}

trait AdditiveGroupAction[P, G] {
  def scalar: AdditiveGroup[G]

  def gplusl(g: G, p: P): P
  def gplusr(p: P, g: G): P = gplusl(g, p)
}

trait MultiplicativeGroupAction[P, G] {
  def scalar: MultiplicativeGroup[G]

  def gtimesl(g: G, p: P): P
  def gtimesr(p: P, g: G): P = gtimesl(g, p)
}

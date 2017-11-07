package spire
package algebra


/**
 * A (left) semigroup/monoid/group action of `G` on `P` is simply the implementation of
 * a method `actl(g, p)`, or `g |+|> p`, such that:
 *
 * 1. `(g |+| h) |+|> p === g |+|> (h |+|> p)` for all `g`, `h` in `G` and `p` in `P`.
 *
 * 2. `id |+|> p === p` for all `p` in `P` (if `id` is defined)
 */
trait LeftAction[@sp(Int) P, G] extends Any {
  def actl(g: G, p: P): P
}

object LeftAction {
  @inline def apply[P, G](G: LeftAction[P, G]): LeftAction[P, G] = G
}

/**
 * A (right) semigroup/monoid/group action of `G` on `P` is simply the implementation of
 * a method `actr(p, g)`, or `p <|+| g`, such that:
 *
 * 1. `p <|+| (g |+| h) === (p <|+| g) <|+| h` for all `g`, `h` in `G` and `p` in `P`.
 *
 * 2. `p <|+| id === p` for all `p` in `P` (if `id` is defined)
 */
trait RightAction[@sp(Int) P, G] extends Any {
  def actr(p: P, g: G): P
}

object RightAction {
  @inline def apply[P, G](G: RightAction[P, G]): RightAction[P, G] = G
}

/**
  * A semigroup/monoid/group action of `G` on `P` is the combination of compatible
  * left and right actions, providing:
  *
  *  - the implementation of a method `actl(g, p)`, or `g |+|> p`, such that:
  *
  * 1. `(g |+| h) |+|> p === g |+|> (h |+|> p)` for all `g`, `h` in `G` and `p` in `P`.
  *
  * 2. `id |+|> p === p` for all `p` in `P` (if `id` is defined)
  *
  *   - the implementation of a method `actr(p, g)`, or `p <|+| g`, such that:
  *
  * 3. `p <|+| (g |+| h) === (p <|+| g) <|+| h` for all `g`, `h` in `G` and `p` in `P`.
  *
  * 4. `p <|+| id === p` for all `p` in `P` (if `id` is defined)
  *
  * In addition, if `G` is a group, left and right actions are compatible:
  *
  * 5. `g |+|> p === p <|+| g.inverse`.
  */
trait Action[@sp(Int) P, G] extends Any with LeftAction[P, G] with RightAction[P, G]

object Action {
  @inline def apply[P, G](G: Action[P, G]): Action[P, G] = G
}

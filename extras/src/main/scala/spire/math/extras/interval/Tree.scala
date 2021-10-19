package spire.math.extras.interval

private[interval] object Tree {

  import java.lang.Long.numberOfLeadingZeros

  @inline final def toPrefix(key: Long): Long = key - Long.MinValue

  @inline final def fromPrefix(key: Long): Long = key + Long.MinValue

  @inline final def unsigned_<(i: Long, j: Long) = (i < j) ^ (i < 0L) ^ (j < 0L)

  @inline final def levelAbove(a: Long, b: Long): Byte =
    (63 - numberOfLeadingZeros(a ^ b)).toByte

  @inline final def maskAbove(prefix: Long, bit: Byte) = {
    // this is not the same as (-1L << (bit + 1)) due to the somewhat strange behavior of the java shift operator
    // -1L << 64 gives -1L, whereas (-1L << 63) << 1 gives 0L like we need
    prefix & ((-1L << bit) << 1)
  }

  @inline final def zeroAt(value: Long, bit: Byte) =
    (value & (1L << bit)) == 0L

  @inline final def hasMatchAt(key: Long, prefix: Long, level: Byte) =
    maskAbove(key, level) == prefix

  def concat(t1: Leaf, t2: Leaf): Tree = {
    if (!unsigned_<(t1.prefix, t2.prefix))
      throw new IllegalArgumentException("Arguments of concat must be ordered")
    val p1 = t1.prefix
    val p2 = t2.prefix
    val l = levelAbove(p1, p2)
    val p = maskAbove(p1, l)
    Branch(p, l, t1, t2)
  }

  //  def join(p1 : Long, t1 : IntervalTrie, p2 : Long, t2 : IntervalTrie) : IntervalTrie = {
  //    val l = levelAbove(p1, p2)
  //    val p = maskAbove(p1, l)
  //    if (zeroAt(p1, l))
  //      Branch(p, l, t1, t2)
  //    else
  //      Branch(p, l, t2, t1)
  //  }

  /**
   * Creates a branch from two possibly null children. In case one of the children is null, the other child will be
   * returned. So if both children are null, this operation can return null.
   * @param p
   *   the prefix to be used for a new branch
   * @param level
   *   the level to be used for a new branch
   * @param l
   *   the left child
   * @param r
   *   the right child
   * @return
   *   the result, can be null
   */
  @inline final private def branch(p: Long, level: Byte, l: Tree, r: Tree): Tree =
    if (l eq null)
      r
    else if (r eq null)
      l
    else
      Branch(p, level, l, r)

  // $COVERAGE-OFF$
  private[extras] def unreachable: Nothing = throw new NotImplementedError("You should never get here")
  // $COVERAGE-ON$

  sealed abstract class BooleanBinaryOperator {

    @inline final private def disjoint(a0: Boolean, a: Tree, b0: Boolean, b: Tree): Boolean = {
      val a_p = a.prefix
      val b_p = b.prefix
      val level = levelAbove(a_p, b_p)
      if (zeroAt(a_p, level)) {
        // a is before b, so it is overlapped by b0
        // b is behind a, so it is overlapped by a0 ^ a.sign
        overlapA(a0, a, b0) && overlapB(a0 ^ a.sign, b0, b)
      } else {
        // b is before a, so it is overlapped by a0
        // a is behind b, so it is overlapped by b0 ^ b.sign
        overlapB(a0, b0, b) && overlapA(a0, a, b0 ^ b.sign)
      }
    }

    protected def op(a: Boolean, b: Boolean): Boolean

    /**
     * This is called if two leaves collide (have the same prefix)
     * @param a0
     *   the value before a
     * @param a
     *   a leaf from the lhs
     * @param b0
     *   the value before b
     * @param b
     *   a leaf from the rhs
     * @return
     *   the result. Can be a leaf or null
     */
    protected def collision(a0: Boolean, a: Leaf, b0: Boolean, b: Leaf): Boolean

    /**
     * This will be called when a is completely covered by a contiguous interval of b
     * @param a0
     * @param a
     *   a non-null tree (leaf or branch)
     * @param b0
     *   the constant value of b in the complete interval of a
     * @return
     *   the result, can be null
     */
    protected def overlapA(a0: Boolean, a: Tree, b0: Boolean): Boolean

    /**
     * This will be called when b is completely covered by a contiguous interval of a
     * @param a0
     *   the constant value of a in the complete interval of b
     * @param b0
     * @param b
     *   a non-null tree (leaf or branch)
     * @return
     *   the result, can be null
     */
    protected def overlapB(a0: Boolean, b0: Boolean, b: Tree): Boolean

    /**
     * Performs the binary operation for two arbitrary trees
     * @param a0
     *   the value before a
     * @param a
     *   a node (leaf or branch) from the lhs
     * @param b0
     *   the value before b
     * @param b
     *   a node (leaf or branch) from the rhs
     * @return
     *   the result, can be null
     */
    final private def op(a0: Boolean, a: Tree, b0: Boolean, b: Tree): Boolean = {
      val a_l = a.level
      val a_p = a.prefix
      val b_l = b.level
      val b_p = b.prefix

      if (a_l > b_l) {
        // a is larger => a must be a branch
        if (!hasMatchAt(b_p, a_p, a_l)) {
          // the prefix of a and b is different. We don't care if a is a branch or a leaf
          disjoint(a0, a, b0, b)
        } else
          a match {
            case a: Branch =>
              val am = a0 ^ a.left.sign
              if (zeroAt(b_p, a_l)) {
                // b fits into the left child of a
                op(a0, a.left, b0, b) && overlapA(am, a.right, b0 ^ b.sign)
              } else {
                // b fits into the right child of a
                overlapA(a0, a.left, b0) && op(am, a.right, b0, b)
              }
            // $COVERAGE-OFF$
            case _ => unreachable
            // $COVERAGE-ON$
          }
      } else if (b_l > a_l) {
        // b is larger => b must be a branch
        if (!hasMatchAt(a_p, b_p, b_l)) {
          // the prefix of a and b is different. We don't care if b is a branch or a leaf
          disjoint(a0, a, b0, b)
        } else
          b match {
            case b: Branch =>
              val bm = b0 ^ b.left.sign
              if (zeroAt(a_p, b_l)) {
                // a fits into the left child of b
                op(a0, a, b0, b.left) && overlapB(a0 ^ a.sign, bm, b.right)
              } else {
                // a fits into the right child of b
                overlapB(a0, b0, b.left) && op(a0, a, bm, b.right)
              }
            // $COVERAGE-OFF$
            case _ => unreachable
            // $COVERAGE-ON$
          }
      } else {
        // a_l == b_l, trees are the same size
        if (a_p == b_p) {
          (a, b) match {
            case (a: Branch, b: Branch) =>
              val am = a0 ^ a.left.sign
              val bm = b0 ^ b.left.sign
              // same prefix. leaves have to be merged
              // todo: check if we can return b unchanged
              op(a0, a.left, b0, b.left) && op(am, a.right, bm, b.right)
            case (a: Leaf, b: Leaf) =>
              collision(a0, a, b0, b)
            // $COVERAGE-OFF$
            case _ => unreachable
            // $COVERAGE-ON$
          }
        } else {
          // same mask, different prefix
          disjoint(a0, a, b0, b)
        }
      }
    }

    final def apply(a0: Boolean, a: Tree, b0: Boolean, b: Tree): Boolean = op(a0, b0) && {
      if ((a eq null) && (b eq null))
        true
      else if (a eq null)
        overlapB(a0, b0, b)
      else if (b eq null)
        overlapA(a0, a, b0)
      else
        op(a0, a, b0, b)
    }
  }

  /**
   * A binary calculator that preserves the order of operands. This is the most generic case. It is used even for
   * symmetric operations. There might be some performance benefit in doing an operator for symmetric operations, but I
   * doubt that it is worth it. And in any case I am too lazy right now.
   */
  sealed abstract class OrderedBinaryOperator {

    /**
     * Joins two non-overlapping leaves
     * @param a0
     *   the value before a
     * @param a
     *   a node (leaf or branch) from the lhs
     * @param b0
     *   the value before b
     * @param b
     *   a node (leaf or branch) from the rhs
     * @return
     *   the result, can be null
     */
    final private def join(a0: Boolean, a: Tree, b0: Boolean, b: Tree): Tree = {
      val a_p = a.prefix
      val b_p = b.prefix
      val level = levelAbove(a_p, b_p)
      val p = maskAbove(a_p, level)
      if (zeroAt(a_p, level)) {
        // a is before b, so it is overlapped by b0
        val a1 = overlapA(a0, a, b0)
        // b is behind a, so it is overlapped by a0 ^ a.sign
        val b1 = overlapB(a0 ^ a.sign, b0, b)
        // make the branch (results can be empty)
        branch(p, level, a1, b1)
      } else {
        // b is before a, so it is overlapped by a0
        val b1 = overlapB(a0, b0, b)
        // a is behind b, so it is overlapped by b0 ^ b.sign
        val a1 = overlapA(a0, a, b0 ^ b.sign)
        // make the branch (results can be empty)
        branch(p, level, b1, a1)
      }
    }

    /**
     * This is called if two leaves collide (have the same prefix)
     * @param a0
     *   the value before a
     * @param a
     *   a leaf from the lhs
     * @param b0
     *   the value before b
     * @param b
     *   a leaf from the rhs
     * @return
     *   the result. Can be a leaf or null
     */
    protected def collision(a0: Boolean, a: Leaf, b0: Boolean, b: Leaf): Tree

    /**
     * This will be called when a is completely covered by a contiguous interval of b
     * @param a0
     * @param a
     *   a non-null tree (leaf or branch)
     * @param b0
     *   the constant value of b in the complete interval of a
     * @return
     *   the result, can be null
     */
    protected def overlapA(a0: Boolean, a: Tree, b0: Boolean): Tree

    /**
     * This will be called when b is completely covered by a contiguous interval of a
     * @param a0
     *   the constant value of a in the complete interval of b
     * @param b0
     * @param b
     *   a non-null tree (leaf or branch)
     * @return
     *   the result, can be null
     */
    protected def overlapB(a0: Boolean, b0: Boolean, b: Tree): Tree

    /**
     * Performs the binary operation for two arbitrary trees
     * @param a0
     *   the value before a
     * @param a
     *   a node (leaf or branch) from the lhs
     * @param b0
     *   the value before b
     * @param b
     *   a node (leaf or branch) from the rhs
     * @return
     *   the result, can be null
     */
    final private def op(a0: Boolean, a: Tree, b0: Boolean, b: Tree): Tree = {
      val a_l = a.level
      val a_p = a.prefix
      val b_l = b.level
      val b_p = b.prefix

      if (a_l > b_l) {
        // a is larger => a must be a branch
        if (!hasMatchAt(b_p, a_p, a_l)) {
          // the prefix of a and b is different. We don't care if a is a branch or a leaf
          join(a0, a, b0, b)
        } else
          a match {
            case a: Branch =>
              val am = a0 ^ a.left.sign
              if (zeroAt(b_p, a_l)) {
                // b fits into the left child of a
                a.lr(op(a0, a.left, b0, b), overlapA(am, a.right, b0 ^ b.sign))
              } else {
                // b fits into the right child of a
                a.lr(overlapA(a0, a.left, b0), op(am, a.right, b0, b))
              }
            // $COVERAGE-OFF$
            case _ => unreachable
            // $COVERAGE-ON$
          }
      } else if (b_l > a_l) {
        // b is larger => b must be a branch
        if (!hasMatchAt(a_p, b_p, b_l)) {
          // the prefix of a and b is different. We don't care if b is a branch or a leaf
          join(a0, a, b0, b)
        } else
          b match {
            case b: Branch =>
              val bm = b0 ^ b.left.sign
              if (zeroAt(a_p, b_l)) {
                // a fits into the left child of b
                b.lr(op(a0, a, b0, b.left), overlapB(a0 ^ a.sign, bm, b.right))
              } else {
                // a fits into the right child of b
                b.lr(overlapB(a0, b0, b.left), op(a0, a, bm, b.right))
              }
            // $COVERAGE-OFF$
            case _ => unreachable
            // $COVERAGE-ON$
          }
      } else {
        // a_l == b_l, trees are the same size
        if (a_p == b_p) {
          (a, b) match {
            case (a: Branch, b: Branch) =>
              val am = a0 ^ a.left.sign
              val bm = b0 ^ b.left.sign
              // same prefix. leaves have to be merged
              // todo: check if we can return b unchanged
              a.lr(op(a0, a.left, b0, b.left), op(am, a.right, bm, b.right))
            case (a: Leaf, b: Leaf) =>
              collision(a0, a, b0, b)
            // $COVERAGE-OFF$
            case _ => unreachable
            // $COVERAGE-ON$
          }
        } else {
          // same mask, different prefix
          join(a0, a, b0, b)
        }
      }
    }

    final def apply(a0: Boolean, a: Tree, b0: Boolean, b: Tree) = {
      if ((a eq null) && (b eq null))
        null
      else if (a eq null)
        overlapB(a0, b0, b)
      else if (b eq null)
        overlapA(a0, a, b0)
      else
        op(a0, a, b0, b)
    }
  }

  object DisjointCalculator extends BooleanBinaryOperator {

    override protected def op(a: Boolean, b: Boolean): Boolean = !(a & b)

    override protected def overlapB(a0: Boolean, b0: Boolean, b: Tree): Boolean = !a0

    override protected def overlapA(a0: Boolean, a: Tree, b0: Boolean): Boolean = !b0

    override protected def collision(a0: Boolean, a: Leaf, b0: Boolean, b: Leaf): Boolean = {
      val at1 = !((a.at ^ a0) & (b.at ^ b0))
      val above1 = !((a.above ^ a0) & (b.above ^ b0))
      at1 && above1
    }
  }

  object SupersetOfCalculator extends BooleanBinaryOperator {

    override protected def op(a: Boolean, b: Boolean): Boolean = a | !b

    override protected def overlapB(a0: Boolean, b0: Boolean, b: Tree): Boolean = a0

    override protected def overlapA(a0: Boolean, a: Tree, b0: Boolean): Boolean = !b0

    override protected def collision(a0: Boolean, a: Leaf, b0: Boolean, b: Leaf): Boolean = {
      val at1 = (a.at ^ a0) | !(b.at ^ b0)
      val above1 = (a.above ^ a0) | !(b.above ^ b0)
      at1 && above1
    }
  }

  object OrCalculator extends OrderedBinaryOperator {

    protected def collision(a0: Boolean, a: Leaf, b0: Boolean, b: Leaf) = {
      val below1 = a0 | b0
      val at1 = (a.at ^ a0) | (b.at ^ b0)
      val above1 = (a.above ^ a0) | (b.above ^ b0)
      leaf(below1 != at1, at1 != above1, a, b)
    }

    protected def overlapA(a0: Boolean, a: Tree, b0: Boolean) =
      if (b0)
        null
      else
        a

    protected def overlapB(a0: Boolean, b0: Boolean, b: Tree) =
      if (a0)
        null
      else
        b
  }

  object XorCalculator extends OrderedBinaryOperator {

    protected def collision(a0: Boolean, a: Leaf, b0: Boolean, b: Leaf) = {
      val below1 = a0 ^ b0
      val at1 = (a.at ^ a0) ^ (b.at ^ b0)
      val above1 = (a.above ^ a0) ^ (b.above ^ b0)
      leaf(below1 != at1, at1 != above1, a, b)
    }

    protected def overlapA(a0: Boolean, a: Tree, b0: Boolean) = a

    protected def overlapB(a0: Boolean, b0: Boolean, b: Tree) = b
  }

  object AndCalculator extends OrderedBinaryOperator {

    protected def collision(a0: Boolean, a: Leaf, b0: Boolean, b: Leaf) = {
      val below1 = a0 & b0
      val at1 = (a.at ^ a0) & (b.at ^ b0)
      val above1 = (a.above ^ a0) & (b.above ^ b0)
      leaf(below1 != at1, at1 != above1, a, b)
    }

    protected def overlapA(a0: Boolean, a: Tree, b0: Boolean) =
      if (b0)
        a
      else
        null

    protected def overlapB(a0: Boolean, b0: Boolean, b: Tree) =
      if (a0)
        b
      else
        null
  }

  /**
   * An operation that calculates the value before, at or behind a position
   */
  sealed abstract class Sampler {

    def apply(a0: Boolean, a: Tree, value: Long) = op(a0, a, value)

    /**
     * Method that is invoked when a leaf is found. This allows to customize whether we want at, before or after
     * @param a0
     *   the value before the leaf
     * @param a
     *   the leaf
     */
    protected def onLeaf(a0: Boolean, a: Leaf): Boolean

    final private def op(a0: Boolean, a: Tree, value: Long): Boolean = a match {
      case a: Branch =>
        val prefix = a.prefix
        val level = a.level
        if (!hasMatchAt(value, prefix, level)) {
          // key is either before or after a
          val branchLevel = levelAbove(prefix, value)
          if (zeroAt(prefix, branchLevel))
            a0 ^ a.sign // after
          else
            a0 // before
        } else {
          // key is within a
          if (zeroAt(value, level))
            op(a0, a.left, value)
          else
            op(a0 ^ a.left.sign, a.right, value)
        }
      case a: Leaf =>
        if (a.prefix == value)
          onLeaf(a0, a)
        else if (unsigned_<(a.prefix, value))
          a0 ^ a.sign
        else
          a0
    }
  }

  object SampleBelow extends Sampler {

    protected def onLeaf(a0: Boolean, a: Leaf): Boolean = a0
  }

  object SampleAt extends Sampler {

    protected def onLeaf(a0: Boolean, a: Leaf): Boolean = a0 ^ a.at
  }

  object SampleAbove extends Sampler {

    protected def onLeaf(a0: Boolean, a: Leaf): Boolean = a0 ^ a.above
  }

  def leaf(changeBelow: Boolean, changeAbove: Boolean, a: Leaf, b: Leaf) = {
    val at = changeBelow
    val sign = changeBelow ^ changeAbove
    if (!changeBelow && !changeAbove)
      null
    else if (at == a.at && sign == a.sign)
      a
    else if (at == b.at && sign == b.sign)
      b
    else
      Leaf(a.prefix, at, sign)
  }

  /**
   * A leaf.
   * @param prefix
   *   the prefix, which in case of a leaf is identical to the key
   */
  final case class Leaf(prefix: Long, at: Boolean, sign: Boolean) extends Tree {

    /**
     * For a leaf, the prefix is the key
     */
    def key = fromPrefix(prefix)

    /**
     * This is -1 for leaves, so that the smallest possible branch (level=0) has a bigger level than a leaf
     */
    def level = -1.toByte

    @inline def above = sign
  }

  /**
   * A branch
   * @param prefix
   *   the common prefix of both children
   * @param level
   *   the level of the node. 0..63 for branches. Higher means bigger
   * @param left
   *   the left child
   * @param right
   *   the right child
   */
  final case class Branch(prefix: Long, level: Byte, left: Tree, right: Tree) extends Tree {

    val sign = left.sign ^ right.sign

    def lr(left: Tree, right: Tree): Tree = {
      if (left eq null)
        right
      else if (right eq null)
        left
      else if ((left eq this.left) && (right eq this.right))
        this
      else
        copy(left = left, right = right)
    }
  }
}

sealed abstract private[interval] class Tree {

  def prefix: Long

  def level: Byte

  def sign: Boolean
}

package spire.math
package expr

import spire.algebra.Ring
import spire.implicits._

trait RingTree extends Tree {
  case class IntPowerNode(base: Node, exponent: Int) extends ParentNode {
    def children = Seq(base)
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newBase) => IntPowerNode(newBase, exponent)
    }
    def isValidCompact = base.isValidCompact
    def isValidDisplay = base.isValidDisplay
  }

  case class AddNode(left: Node, right: Node) extends DisplayBinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => AddNode(newLeft, newRight)
    }
  }
  case class SubNode(left: Node, right: Node) extends DisplayBinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => SubNode(newLeft, newRight)
    }
  }
  case class MulNode(left: Node, right: Node) extends DisplayBinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => MulNode(newLeft, newRight)
    }
  }
  case class PlusNode(args: Seq[Node]) extends SeqNode {
    def build(newChildren: Seq[Node]) = PlusNode(newChildren)
  }
  case class TimesNode(args: Seq[Node]) extends SeqNode {
    def build(newChildren: Seq[Node]) = TimesNode(newChildren)
  }

  /** Negation/opposite. */
  case class NegNode(node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => NegNode(n)
    }
    def isValidCompact = node.isValidCompact
    def isValidDisplay = node.isValidDisplay
  }

  def zeroNode: Node
  def oneNode: Node
  def nodeFromInt(i: Int): Node
  def nodeToInt(n: Node): Option[Int]

  override def toCompact(displayNode: Node): Node = displayNode match {
    case NegNode(node) => -toCompact(node)
    case AddNode(left, right) => toCompact(left) + toCompact(right)
    case SubNode(left, right) => toCompact(left) - toCompact(right)
    case MulNode(left, right) => toCompact(left) * toCompact(right)
    case IntPowerNode(base, exponent) => toCompact(base) ** exponent
    case _ => super.toCompact(displayNode)
  }

  override def toDisplay(compactNode: Node): Node = compactNode match {
    case PlusNode(Seq(head, tail@_*)) => (toDisplay(head) /: tail) {
      case (displayNode, node) => toDisplay(node) match {
        case NegNode(n) => SubNode(displayNode, n)
        case n => AddNode(displayNode, n)
      }
    }
    case NegNode(node) => NegNode(toDisplay(node))
    case TimesNode(Seq(head, tail@_*)) => (toDisplay(head) /: tail) {
      case (displayNode, node) => MulNode(displayNode, toDisplay(node))
    }
    case IntPowerNode(base, exponent) => IntPowerNode(toDisplay(base), exponent)
    case _ => super.toDisplay(compactNode)
  }

  def expanded(compactNode: Node): Node =
    tryExpanded(compactNode).map(expanded(_)).getOrElse(compactNode)

  def tryExpanded(compactNode: Node): Option[Node] = {
    // can we expand the children ?
    compactNode match {
      case parent: ParentNode =>
        val expandedChildren: Seq[Option[Node]] = parent.children.map(tryExpanded(_))
        if (expandedChildren.exists(_.nonEmpty)) {
          val newChildren = (expandedChildren zip parent.children) map {
            case (Some(x), _) => x
            case (None, y) => y
          }
          return Some(parent.build(newChildren))
        }
      case _ =>
    }
    compactNode match {
      case IntPowerNode(base, exponent) => return Some(TimesNode(Seq.fill(exponent)(base)))
      case TimesNode(seq) if seq.exists(_.isInstanceOf[PlusNode]) =>
        val (before, Seq(PlusNode(terms), after@_*)) = seq.span(!_.isInstanceOf[PlusNode])
        return Some(PlusNode(terms.map(term => TimesNode((before :+ term) ++ after))))
      case _ =>
    }
    None
  }

  override def trySimplified(compactNode: Node): Option[Node] = {
    compactNode match {
      case TimesNode(seq) if seq.exists(_.isInstanceOf[TimesNode]) =>
        val (before, Seq(TimesNode(terms), after@_*)) = seq.span(!_.isInstanceOf[TimesNode])
        return Some(TimesNode(before ++ terms ++ after))
      case PlusNode(seq) if seq.exists(_.isInstanceOf[PlusNode]) =>
        val (before, Seq(PlusNode(terms), after@_*)) = seq.span(!_.isInstanceOf[PlusNode])
        return Some(PlusNode(before ++ terms ++ after))
      case TimesNode(Seq()) => return Some(oneNode)
      case PlusNode(Seq()) => return Some(zeroNode)
      case TimesNode(Seq(one)) => return Some(one)
      case PlusNode(Seq(one)) => return Some(one)
      case NegNode(NegNode(node)) => return Some(node)
      case NegNode(PlusNode(seq)) => return Some(PlusNode(seq.map(-_)))
      case TimesNode(seq) if seq.exists(_.isInstanceOf[NegNode]) =>
        val count = seq.count(_.isInstanceOf[NegNode])
        val newTimesNode = TimesNode(seq.map {
          case NegNode(node) => node
          case node => node
        })
        Some(if (count % 2 == 0) newTimesNode else NegNode(newTimesNode))
      case _ =>
    }
    super.trySimplified(compactNode)
  }

  class NodeRing extends Ring[Node] {
    def zero = zeroNode
    def one = oneNode
    override def fromInt(n: Int) = nodeFromInt(n)

    def negate(x: Node): Node = x match {
      case NegNode(node) => node
      case PlusNode(seq) => PlusNode(seq.map(negate(_)))
      case _ => NegNode(x)
    }

    def plus(x: Node, y: Node) = (x, y) match {
      case (PlusNode(a), PlusNode(b)) => PlusNode(a ++ b)
      case (PlusNode(a), b) => PlusNode(a :+ b)
      case (a, PlusNode(b)) => PlusNode(a +: b)
      case (a, b) => PlusNode(Seq(a, b))
    }

    override def minus(x: Node, y: Node): Node = (x, y) match {
      case (PlusNode(a), PlusNode(b)) => PlusNode(a ++ b.map(negate(_)))
      case (PlusNode(a), b) => PlusNode(a :+ negate(b))
      case (a, PlusNode(b)) => PlusNode(a +: b.map(negate(_)))
      case (a, b) => PlusNode(Seq(a, negate(b)))
    }

    def times(x: Node, y: Node): Node = (x, y) match {
      case (TimesNode(a), TimesNode(b)) => TimesNode(a ++ b)
      case (TimesNode(a), NegNode(b)) => negate(TimesNode(a :+ b))
      case (NegNode(a), TimesNode(b)) => negate(TimesNode(a +: b))
      case (NegNode(a), NegNode(b)) => TimesNode(Seq(a, b))
      case (a, NegNode(b)) => negate(TimesNode(Seq(a, b)))
      case (NegNode(a), b) => negate(TimesNode(Seq(a, b)))
      case (a, b) => TimesNode(Seq(a, b))
    }

    override def pow(base: Node, exponent: Int): Node = exponent match {
      case 0 => oneNode
      case 1 => base
      case _ => IntPowerNode(base, exponent)
    }
  }

  implicit def nodeRing: Ring[Node] = new NodeRing

  def TreeParser: RingTreeParserTrait
  def TreeUnparser: RingTreeUnparserTrait

  trait RingTreeParserTrait extends TreeParserTrait {
    def buildPowerNode(base: Node, exponent: Node): Node =
      IntPowerNode(base, nodeToInt(exponent).getOrElse(sys.error(s"Only integer powers are supported and $exponent is not an integer")))

    lazy val p_op: PackratParser[Node] =
      ((primary <~ "**") ~ u_expr) ^^ {
        case base ~ exponent => buildPowerNode(base, exponent)
      }

    lazy val u_op: PackratParser[Node] =
      (("-" ~> u_expr) ^^ (n => NegNode(n))) |||
      (("+" ~> u_expr) ^^ identity)

    lazy val m_op: PackratParser[Node] =
      ((m_expr <~ "*") ~ u_expr) ^^ { case x ~ y => MulNode(x, y) }

    lazy val a_op: PackratParser[Node] =
      ((a_expr <~ "+") ~ m_expr) ^^ { case x ~ y => AddNode(x, y) }

    lazy val s_op: PackratParser[Node] =
      ((a_expr <~ "-") ~ m_expr) ^^ { case x ~ y => SubNode(x, y) }
  }

  trait RingTreeUnparserTrait extends TreeUnparserTrait {
    override def printable(n: Node): Printable = n match {
      case IntPowerNode(base, exponent) => RightAssoc(base, "**", nodeFromInt(exponent), priority = 40)
      case NegNode(node) => Prefix("-", node, priority = 30)
      case AddNode(left, right) => LeftAssoc(left, "+", right, priority = 10)
      case SubNode(left, right) => LeftAssoc(left, "-", right, priority = 10)
      case MulNode(left, right) => LeftAssoc(left, "*", right, priority = 20)
      case _ => super.printable(n)
    }
  }
}

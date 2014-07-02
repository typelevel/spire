package spire.math
package expr

/** A trait for expression trees that can be parsed and pretty-printed. */
trait Tree {
  import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
  import spire.algebra.{Field, NRoot}
  import spire.implicits._
  /** A node in an expression tree. */
  sealed trait Node

  /** An atomic node in an expression tree, which can be
    * used for values or symbols. Should be extended. */
  trait AtomNode extends Node

  /** Node with children. */
  sealed trait ParentNode extends Node {
    def children: Seq[Node]
    def build(newChildren: Seq[Node]): ParentNode
  }

  /** A node representing a binary operation.
    * 
    * Some operations, such as +, -, *, / can be represented either
    * using binary operations, or by using a sum or product of a sequence of
    * terms. The binary form is used during parsing/pretty-printing, while
    * the sequence form is used to perform simplifications.
    * 
    * The power operator ^ can only be represented as a binary operator.
    */
  sealed trait BinaryNode extends ParentNode {
    def children = Seq(left, right)
    def left: Node
    def right: Node
  }

  /** Binary nodes used only to display/parse expressions. */
  sealed trait DisplayBinaryNode extends BinaryNode
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
  case class DivNode(left: Node, right: Node) extends DisplayBinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => DivNode(newLeft, newRight)
    }
  }

  /** A node representing left to the power right. */
  case class PowerNode(left: Node, right: Node) extends BinaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(newLeft, newRight) => PowerNode(newLeft, newRight)
    }
  }

  /** Sequence node representing a sum or a product of terms.
    * 
    * Subtraction and division are handled by adding the opposite
    * or multiplying by the inverse.
    *
    * These nodes are used during simplifications, and must be
    * converted back to binary operators to be pretty-printed.
    */
  sealed trait SeqNode extends ParentNode {
    def children = args
    def build(newChildren: Seq[Node]): SeqNode
    def args: Seq[Node]
  }

  case class PlusNode(args: Seq[Node]) extends SeqNode {
    def build(newChildren: Seq[Node]) = PlusNode(newChildren)
  }
  case class TimesNode(args: Seq[Node]) extends SeqNode {
    def build(newChildren: Seq[Node]) = TimesNode(newChildren)
  }

  /** Node representing unary operations. */
  sealed trait UnaryNode extends ParentNode {
    def node: Node
    def children = Seq(node)
  }
  /** Negation/opposite. */
  case class NegNode(node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => NegNode(n)
    }
  }
  /** Inverse/reciprocal. */
  case class InvNode(node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => InvNode(n)
    }
  }
  /** Unary function. */
  case class FunNode(name: String, node: Node) extends UnaryNode {
    def build(newChildren: Seq[Node]) = newChildren match {
      case Seq(n) => FunNode(name, n)
    }
  }

  implicit val NodeField: NodeFieldTrait

  trait NodeFieldTrait extends Field[Node] with NRoot[Node] {
    def gcd(a: Node, b: Node): Node =
      sys.error("Not implemented")
    def mod(a: Node, b: Node): Node =
      sys.error("Not implemented")
    def quot(a: Node,b: Node): Node =
      sys.error("Not implemented")

    // to be implemented
    def zero: Node
    def one: Node

    def negate(x: Node): Node = x match {
      case NegNode(node) => node
      case PlusNode(seq) => PlusNode(seq.map(negate(_)))
      case _ => NegNode(x)
    }

    override def reciprocal(x: Node): Node = x match {
      case InvNode(node) => node
      case TimesNode(seq) => TimesNode(seq.map(reciprocal(_)))
      case _ => InvNode(x)
    }

    def plus(x: Node, y: Node): Node = (x, y) match {
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

    def div(x: Node, y: Node): Node = (x, y) match {
      case (TimesNode(a), TimesNode(b)) => TimesNode(a ++ b.map(_.reciprocal))
      case (TimesNode(a), b) => TimesNode(a :+ (b.reciprocal))
      case (a, TimesNode(b)) => TimesNode(a +: b.map(_.reciprocal))
      case (a, b) => TimesNode(Seq(a, b.reciprocal))
    }

    def fpow(x: Node, y: Node): Node = PowerNode(x, y)
    def nroot(a: Node, n: Int): Node = PowerNode(a, fromInt(n).reciprocal)
  }

  /** Transform the node to the compact form by changing BinaryNode
    * to SeqNode when possible. 
    */
  def compact(displayNode: Node): Node = displayNode match {
    case _: SeqNode => sys.error("Node is not a display node.")
    case atom: AtomNode => atom // do nothing
    case AddNode(left, right) => compact(left) + compact(right)
    case SubNode(left, right) => compact(left) - compact(right)
    case MulNode(left, right) => compact(left) * compact(right)
    case DivNode(left, right) => compact(left) / compact(right)
    case PowerNode(left, right) => compact(left).fpow(compact(right))
    case NegNode(node) => -compact(node)
    case InvNode(node) => compact(node).reciprocal
    case FunNode(name, node) => FunNode(name, compact(node))
  }

  def displayAtom(atom: AtomNode): Node
  /** Transform the node to the display form by changing all SeqNode to
    * BinaryNode.
    */
  def display(compactNode: Node): Node = compactNode match {
    case atom: AtomNode => displayAtom(atom) // do nothing
    case PowerNode(left, right) => PowerNode(display(left), display(right))
    case _: DisplayBinaryNode => sys.error("Node is not a compact node.")
    case PlusNode(Seq(head, tail@_*)) => (display(head) /: tail) {
      case (displayNode, node) => display(node) match {
        case NegNode(n) => SubNode(displayNode, n)
        case n => AddNode(displayNode, n)
      }
    }
    case TimesNode(Seq(head, tail@_*)) => (display(head) /: tail) {
      case (displayNode, node) => display(node) match {
        case InvNode(n) => DivNode(displayNode, n)
        case n => MulNode(displayNode, n)
      }
    }
    case InvNode(node) => DivNode(NodeField.one, display(node))
    case NegNode(node) => NegNode(display(node))
    case FunNode(name, node) => FunNode(name, display(node))
  }

  def simplified(compactNode: Node): Node = 
    trySimplify(compactNode).map(simplified(_)).getOrElse(compactNode)
  def trySimplify(compactNode: Node): Option[Node] = {
    // can we simplify the children ?
    compactNode match {
      case parent: ParentNode =>
        val simplifiedChildren: Seq[Option[Node]] = parent.children.map(trySimplify(_))
        if (simplifiedChildren.exists(_.nonEmpty)) {
          val newChildren = (simplifiedChildren zip parent.children) map {
            case (Some(x), _) => x
            case (None, y) => y
          }
          return Some(parent.build(newChildren))
        }
      case _ =>
    }
    // basic simplification rules
    compactNode match {
      case TimesNode(Seq()) => return Some(NodeField.one)
      case PlusNode(Seq()) => return Some(NodeField.zero)
      case TimesNode(Seq(node)) => return Some(node)
      case PlusNode(Seq(node)) => return Some(node)
      case NegNode(NegNode(node)) => return Some(node)
      case InvNode(InvNode(node)) => return Some(node)
      case InvNode(NegNode(node)) => return Some(NegNode(InvNode(node)))
      case InvNode(TimesNode(seq)) => return Some(TimesNode(seq.map(_.reciprocal)))
      case NegNode(PlusNode(seq)) => return Some(PlusNode(seq.map(-_)))
      case _ =>
    }
    return None
  }

  /** Parser for an expression. */
  val TreeParser: TreeParserTrait
  /** Pretty-printer for an expression. */
  val TreeUnparser: TreeUnparserTrait

  def parse(s: String): TreeParser.ParseResult[Node] = TreeParser.parse(s).map(compact(_))
  def print(n: Node): String = TreeUnparser.unparse(display(n))

  trait TreeParserTrait extends RegexParsers with PackratParsers {
    import scala.util.parsing.input.CharSequenceReader
    def parse(s: String): ParseResult[Node] =
      phrase(a_expr)(new PackratReader(new CharSequenceReader(s)))

    // grammar adapted from the Python grammar:
    // https://docs.python.org/3/reference/expressions.html
    lazy val primary: PackratParser[Node] = atom | call | parenth_form
    def atom: Parser[Node]

    lazy val enclosure: PackratParser[Node] = parenth_form

    lazy val power: PackratParser[Node] = primary |||
    ((primary <~ "^") ~ u_expr) ^^ {
      case left ~ right => PowerNode(left, right)
    }

    lazy val u_expr: PackratParser[Node] = power |||
    (("-" ~> u_expr) ^^ (n => NegNode(n))) |||
    (("+" ~> u_expr) ^^ identity)

    lazy val m_expr: PackratParser[Node] = u_expr |||
    ((m_expr <~ "*") ~ u_expr) ^^ { case x ~ y => MulNode(x, y) } |||
    ((m_expr <~ "/") ~ u_expr) ^^ { case x ~ y => DivNode(x, y) }

    lazy val a_expr: PackratParser[Node] = m_expr |||
    ((a_expr <~ "+") ~ m_expr) ^^ { case x ~ y => AddNode(x, y) } |||
    ((a_expr <~ "-") ~ m_expr) ^^ { case x ~ y => SubNode(x, y) }

    lazy val call: PackratParser[Node] = identifier ~ (("(" ~> a_expr) <~ ")") ^^ {
      case name ~ node => FunNode(name, node)
    }

    def identifier = """[a-zA-z]+[0-9a-zA-z]*""".r

    lazy val parenth_form: PackratParser[Node] = "(" ~> a_expr <~ ")"
  }

  trait TreeUnparserTrait extends Unparser[Node] {
    def printAtom(n: AtomNode): String
    def printable(n: Node): Printable = n match {
      case atom: AtomNode => Value(printAtom(atom))
      case n: NegNode => Prefix("-", n.node, priority = 30)
      case n: InvNode => sys.error("Cannot display")
      case n: SeqNode => sys.error("Cannot display")
      case AddNode(left, right) => LeftAssoc(left, "+", right, priority = 10)
      case SubNode(left, right) => LeftAssoc(left, "-", right, priority = 10)
      case MulNode(left, right) => LeftAssoc(left, "*", right, priority = 20)
      case DivNode(left, right) => LeftAssoc(left, "/", right, priority = 20)
      case pn: PowerNode => RightAssoc(pn.left, "^", pn.right, priority = 40)
      case f: FunNode => Enclosed(f.name + "(", f.node, ")")
    }
  }
}

package spire.math
package expr

/** A trait for expression trees that can be parsed and pretty-printed. */
trait Tree {
  import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}

  /** A node in an expression tree. */
  sealed trait Node {
    def tree = Tree.this
    def isValidDisplay: Boolean
    def isValidCompact: Boolean
  }

  /** An atomic node in an expression tree, which can be
    * used for values or symbols. Should be extended. */
  trait AtomNode extends Node {
    def isValidCompact = true
    def isValidDisplay = true
  }

  /** Node with children. */
  trait ParentNode extends Node {
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
  trait BinaryNode extends ParentNode {
    def children = Seq(left, right)
    def left: Node
    def right: Node
  }

  /** Binary nodes used only to display/parse expressions. */
  trait DisplayBinaryNode extends BinaryNode {
    def isValidDisplay = left.isValidDisplay && right.isValidDisplay
    def isValidCompact = false
  }

  /** Sequence node representing a sum or a product of terms.
    * 
    * Subtraction and division are handled by adding the opposite
    * or multiplying by the inverse.
    *
    * These nodes are used during simplifications, and must be
    * converted back to binary operators to be pretty-printed.
    */
  trait SeqNode extends ParentNode {
    def children = args
    def build(newChildren: Seq[Node]): SeqNode
    def args: Seq[Node]
    def isValidDisplay = false
    def isValidCompact = children.forall(_.isValidCompact)
  }

  /** Node representing unary operations. */
  trait UnaryNode extends ParentNode {
    def node: Node
    def children = Seq(node)
  }

  /** Transform the node to the compact form by changing BinaryNode
    * to SeqNode when possible.
    */
  def toCompact(displayNode: Node): Node = displayNode match {
    case _: SeqNode => sys.error(s"Node $displayNode is not a display node.")
    case _ => sys.error(s"Unsupported node $displayNode")
  }

  /** Transform the node to the display form by changing all SeqNode to
    * BinaryNode.
    */
  def toDisplay(compactNode: Node): Node = compactNode match {
    case _: DisplayBinaryNode => sys.error(s"Node $compactNode is not a compact node.")
    case _ => sys.error(s"Unsupported node $compactNode")
  }

  /** Basic fast simplifications of the expression. */
  def simplified(compactNode: Node): Node =
    trySimplified(compactNode).map(simplified(_)).getOrElse(compactNode)

  def trySimplified(compactNode: Node): Option[Node] = {
    // can we simplify the children ?
    compactNode match {
      case parent: ParentNode =>
        val simplifiedChildren: Seq[Option[Node]] = parent.children.map(trySimplified(_))
        if (simplifiedChildren.exists(_.nonEmpty)) {
          val newChildren = (simplifiedChildren zip parent.children) map {
            case (Some(x), _) => x
            case (None, y) => y
          }
          return Some(parent.build(newChildren))
        }
      case _ =>
    }
    None
  }

  def parse(str: String): Parsers#ParseResult[Node] = TreeParser.parse(str).map(toCompact(_))
  def print(node: Node): String = TreeUnparser.unparse(toDisplay(node))

  /** Parser for an expression. */
  def TreeParser: TreeParserTrait

  /** Pretty-printer for an expression. */
  def TreeUnparser: TreeUnparserTrait

  trait TreeParserTrait extends RegexParsers with PackratParsers {
    import scala.util.parsing.input.CharSequenceReader

    def parse(s: String): ParseResult[Node] =
      phrase(a_expr)(new PackratReader(new CharSequenceReader(s)))

    // grammar adapted from the Python grammar:
    // https://docs.python.org/3/reference/expressions.html
    lazy val primary: PackratParser[Node] = atom | call | parenth_form
    def atom: Parser[Node]

    lazy val enclosure: PackratParser[Node] = parenth_form

    lazy val p_expr: PackratParser[Node] = primary |||
      p_op

    def p_op: PackratParser[Node]

    lazy val u_expr: PackratParser[Node] = p_expr |||
      u_op

    /** Unary operators */
    def u_op: PackratParser[Node]

    lazy val m_expr: PackratParser[Node] = u_expr |||
      (m_op | d_op)

    /** Multiplication */
    def m_op: PackratParser[Node]
    /** Division */
    def d_op: PackratParser[Node]


    lazy val a_expr: PackratParser[Node] = m_expr |||
      (a_op | s_op)

    /** Addition */
    def a_op: PackratParser[Node]

    /** Subtraction */
    def s_op: PackratParser[Node]

    def call: PackratParser[Node]

    lazy val parenth_form: PackratParser[Node] = "(" ~> a_expr <~ ")"
  }

  trait TreeUnparserTrait extends Unparser[Node] {
    def printable(n: Node): Printable = n match {
      case _: SeqNode => sys.error("SeqNode is used for compact forms, not for display.")
      case _ => sys.error(s"Unrecognized node $n")
    }
  }
}

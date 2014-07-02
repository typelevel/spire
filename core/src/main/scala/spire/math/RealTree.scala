package spire.math

object RealTree extends ValueTree[Real] {
  import spire.algebra.Trig
  import spire.implicits._

  def value(node: Node): Real = node match {
    case FloatNode(bd, powerOfTen) => Real(bd) * Real(10).fpow(powerOfTen)
    case RationalNode(r) => Real(r)
    case PlusNode(seq) => seq.map(value(_)).reduce(_+_)
    case TimesNode(seq) => seq.map(value(_)).reduce(_*_)
    case InvNode(node) => value(node).reciprocal
    case NegNode(node) => -value(node)
    case PowerNode(left, right) => value(left).fpow(value(right))

    case ConstantNode("e") => Trig[Real].e
    case ConstantNode("pi") => Trig[Real].pi
    case c: ConstantNode => sys.error(s"Invalid constant $c")
    case a: AtomNode => sys.error("Should not happen")
    case b: DisplayBinaryNode => sys.error(s"Node $b is in display format")

    case FunNode("exp", node) => Trig[Real].exp(value(node))
    case FunNode("expm1", node) => Trig[Real].expm1(value(node))
    case FunNode("log", node) => Trig[Real].log(value(node))
    case FunNode("log1p", node) => Trig[Real].log1p(value(node))

    case FunNode("sin", node) => Trig[Real].sin(value(node))
    case FunNode("cos", node) => Trig[Real].cos(value(node))
    case FunNode("tan", node) => Trig[Real].tan(value(node))

    case FunNode("asin", node) => Trig[Real].asin(value(node))
    case FunNode("acos", node) => Trig[Real].acos(value(node))
    case FunNode("atan", node) => Trig[Real].atan(value(node))
      // atan2 not supported yet
    case FunNode("sinh", node) => Trig[Real].sinh(value(node))
    case FunNode("cosh", node) => Trig[Real].cosh(value(node))
    case FunNode("tanh", node) => Trig[Real].tanh(value(node))

    case FunNode("toRadians", node) => Trig[Real].toRadians(value(node))
    case FunNode("toDegrees", node) => Trig[Real].toDegrees(value(node))
    case f: FunNode => sys.error(s"Invalid function $f")
  }

  case class RationalNode(r: Rational) extends AtomNode
  case class FloatNode(bd: BigDecimal, powerOfTen: Int) extends AtomNode
  case class ConstantNode(name: String) extends AtomNode

  def rationalValue(node: Node): Rational = node match {
    case _: FloatNode => sys.error("Cannot transform a FloatNode to Rational.")
    case RationalNode(r) => r
    case PlusNode(seq) => seq.map(rationalValue(_)).reduce(_+_)
    case TimesNode(seq) => seq.map(rationalValue(_)).reduce(_*_)
    case NegNode(n) => -rationalValue(n)
    case InvNode(n) => rationalValue(n).reciprocal
    case _ => sys.error("Cannot transform node to Rational.")
  }

  override def trySimplify(compactNode: Node): Option[Node] = {
    compactNode match {
      case NegNode(RationalNode(r)) => return Some(RationalNode(-r))
      case InvNode(RationalNode(r)) => return Some(RationalNode(r.reciprocal))
      case NegNode(FloatNode(bd, powerOfTen)) => return Some(FloatNode(-bd, powerOfTen))
      case PlusNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
        val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
        val r = rationalValue(PlusNode(rational))
        if (r != 0)
          return Some(PlusNode(nonRational :+ RationalNode(r)))
        else
          return Some(PlusNode(nonRational))
      case TimesNode(seq) if seq.count(_.isInstanceOf[RationalNode]) > 1 =>
        val (rational, nonRational) = seq.partition(_.isInstanceOf[RationalNode])
        val r = rationalValue(TimesNode(rational))
        if (r == 0)
          return Some(RationalNode(r))
        if (r != 1)
          return Some(TimesNode(nonRational :+ RationalNode(r)))
        else
          return Some(TimesNode(nonRational))
      case _ =>
    }
    super.trySimplify(compactNode)
  }

  object NodeField extends NodeFieldTrait {
    def zero = RationalNode(Rational.zero)
    def one = RationalNode(Rational.one)
    override def fromInt(n: Int): Node = RationalNode(Rational(n))
  }

  def displayAtom(atom: AtomNode): Node = atom match {
    case c: ConstantNode => c
    case RationalNode(r) if r < 0 => 
      NegNode(displayAtom(RationalNode(-r)))
    case RationalNode(r) if r.denominator != 1 =>
      DivNode(RationalNode(r.numerator), RationalNode(r.denominator))
    case r: RationalNode => r
    case FloatNode(bd, powerOfTen) if bd < 0 =>
      NegNode(FloatNode(-bd, powerOfTen))
    case f: FloatNode => f
  }

  object TreeParser extends TreeParserTrait {
    lazy val atom: PackratParser[Node] = float | integer | constant
    lazy val constant: PackratParser[Node] = 
      ("pi" ^^^ ConstantNode("pi")) |
      ("e" ^^^ ConstantNode("e"))
    lazy val float: PackratParser[Node] =
      (bigdecimal ~ opt(("e" | "E") ~> int))  ^^ { 
        case bd ~ None => FloatNode(bd, 0)
        case bd ~ Some(exp) => FloatNode(bd, exp)
      }
    lazy val integer: PackratParser[Node] =
      biginteger ^^ { case i => RationalNode(Rational(i)) }
    val int = """-?(0|([1-9]\d*))""".r ^^ { case s: String => s.toInt }
    val biginteger = """0|([1-9]\d*)""".r ^^ { case s: String => BigInt(s) }
    val bigdecimal = """(0|([1-9]\d*))?[.]\d*""".r ^^ { case s: String => BigDecimal(s) }
  }
  object TreeUnparser extends TreeUnparserTrait {
    // force printing the BigDecimal with a dot
    def printBigDecimal(bd: BigDecimal): String = bd.scale match {
      case 0 => bd.toString + "."
      case _ => bd.toString
    }
    def printAtom(a: AtomNode): String = a match {
      case ConstantNode(name) => name
      case RationalNode(r) =>
        require(r >= 0)
        require(r.denominator == 1)
        r.toString
      case FloatNode(bd, 0) => 
        require(bd >= 0)
        printBigDecimal(bd)
      case FloatNode(bd, powerOfTen) =>
        require(bd >= 0)
        s"${printBigDecimal(bd)}e${powerOfTen}"
    }
  }
}

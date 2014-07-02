package spire.math
package expr

trait PolynomialTree extends RingTree {
  case class Variable(power: Int) extends AtomNode

  override def toCompact(displayNode: Node): Node = displayNode match {
    case IntPowerNode(Variable(power), exponent) => return Variable(power * exponent)
    case v: Variable => v
    case _ => super.toCompact(displayNode)
  }

  override def toDisplay(compactNode: Node): Node = compactNode match {
    case Variable(power) if power != 1 => IntPowerNode(Variable(1), power)
    case v: Variable => v
    case _ => super.toDisplay(compactNode)
  }

  override def trySimplified(compactNode: Node): Option[Node] = {
    compactNode match {
      case IntPowerNode(Variable(power), exponent) => return Some(Variable(power * exponent))
      case TimesNode(seq) if seq.count(_.isInstanceOf[Variable]) > 1 =>
        val (variables, nonVariables) = seq.partition(_.isInstanceOf[Variable])
        val exponent = variables.map(_.asInstanceOf[Variable].power).reduce(_+_)
        return Some(TimesNode(nonVariables :+ Variable(exponent)))
      case _ =>
    }
    super.trySimplified(compactNode)
  }

  def TreeParser: PolynomialTreeParserTrait
  def TreeUnparser: PolynomialTreeUnparserTrait

  trait PolynomialTreeParserTrait extends RingTreeParserTrait {
    lazy val variable: PackratParser[Node] = "x" ^^^ Variable(1)
  }
  trait PolynomialTreeUnparserTrait extends RingTreeUnparserTrait {
    override def printable(n: Node): Printable = n match {
      case Variable(1) =>
        Value("x")
      case _ => super.printable(n)
    }
  }
}

package spire.math

/** The sides that an expression may appear on inside another expression. */
sealed abstract class Side
case object LeftSide extends Side
case object RightSide extends Side
case object NoSide extends Side

/** Pretty-printer with support for pretty-printing expressions with minimal
  * parenthesisation.
  *
  * Based on algorithm in "Unparsing expressions with prefix and postfix operators",
  * Ramsey, SP&E, 28 (12), October 1998.
  * 
  * Inspired by the Scala Kiama library and rewritten as a type class.
  */
trait Unparser[E] {

  /** Description of the printed representation of an expression node. */
  sealed abstract class Printable {
    /** Prints the expression, with or without spacing its elements. */
    def print(space: Boolean): String
    /** Adds, when needed, parentheses to this operator expression appearing
      * on some side of an outer operator expression. */
    def bracket(outer: Operator, side: Side, space: Boolean): String
  }

  object Operator {
    /**
      * Return false if the inner expression should be parenthesised when appearing
      * on the given side with the outer expression.
      */
    def needsParentheses(inner: Operator, outer: Operator, side: Side): Boolean = {
      val pi = inner.priority
      val po = outer.priority
      val result = (pi < po) && ((inner, outer, side) match {
        case (_: Postfix, _, LeftSide) => false
        case (_: Prefix, _, RightSide) => false
        case (_: LeftAssoc, _: LeftAssoc, LeftSide) if pi == po => false
        case (_: RightAssoc, _: RightAssoc, RightSide) if pi == po => false
        case (_: LeftAssoc, _: LeftAssoc, NoSide) => false
        case (_: RightAssoc, _: RightAssoc, NoSide) => false
        case (_: NonAssoc, _: NonAssoc, NoSide) => false
        case _ => true
      })
      assert(result == !noparens(inner, outer, side))
      result
    }
    /**
      * Return false if the inner expression should be parenthesised when appearing
      * on the given side with the outer expression.
      */
    def noparens(inner: Operator, outer: Operator, side: Side): Boolean = {
      val pi = inner.priority
      val po = outer.priority
      (pi >= po) || ((inner, outer, side) match {
        case (_: Postfix, _, LeftSide) => true
        case (_: Prefix, _, RightSide) => true
        case (_: LeftAssoc, _: LeftAssoc, LeftSide) if pi == po => true
        case (_: RightAssoc, _: RightAssoc, RightSide) if pi == po => true
        case (_: LeftAssoc, _: LeftAssoc, NoSide) => true
        case (_: RightAssoc, _: RightAssoc, NoSide) => true
        case (_: NonAssoc, _: NonAssoc, NoSide) => true
        case _ => false
      })
    }
  }

  sealed abstract class Atom extends Printable {
    // Atomic elements do not need parentheses
    def bracket(outer: Operator, side: Side, space: Boolean) = print(space)
  }
  case class Value(string: String) extends Atom {
    def print(space: Boolean) = string
  }
  case class Enclosed(before: String, arg: E, after: String) extends Atom {
    def print(space: Boolean) = before + printable(arg).print(space) + after
  }

  /** Element with an operator of some fixity (prefix, postfix, infix) and a priority. */
  sealed abstract class Operator extends Printable {
    /** Operator symbol used to print it. */
    def symbol: String
    /** Priority of the operator. Higher numbers bind more tightly. */
    def priority: Int

    def bracket(outer: Operator, side: Side, space: Boolean): String = {
      val d = print(space)
      if (Operator.needsParentheses(this, outer, side)) "(" + d + ")" else d
    }
  }

  /** Unary operator. */
  sealed abstract class Unary extends Operator {
    def operand: E
    /** Appends or prepends the operator symbol to the expression represented by the string. */
    def withSymbol(expression: String): String
    def print(space: Boolean) = {
      val ed = printable(operand).bracket(this, NoSide, space)
      withSymbol(ed)
    }
  }
  /** The unary operator occurs in prefix position (i.e., before its operand). */
  case class Prefix(symbol: String, operand: E, priority: Int) extends Unary {
    def withSymbol(expression: String) = symbol + expression
  }
  /** The unary operator occurs in postfix position (i.e., after its operand). */
  case class Postfix(operand: E, symbol: String, priority: Int) extends Unary {
    def withSymbol(expression: String) = expression + symbol
  }

  sealed abstract class Binary extends Operator {
    def left: E
    def right: E
  }
  /** Binary operator in the infix position (i.e., between its two operands). */
  sealed abstract class Infix extends Binary {
    def print(space: Boolean) = {
      val ld = printable(left).bracket(this, LeftSide, space)
      val rd = printable(right).bracket(this, RightSide, space)
      val sp = space match { case true => " "; case false => "" }
      ld + sp + symbol + sp + rd
    }
  }
  case class LeftAssoc(left: E, symbol: String, right: E, priority: Int) extends Infix
  case class RightAssoc(left: E, symbol: String, right: E, priority: Int) extends Infix
  case class NonAssoc(left: E, symbol: String, right: E, priority: Int) extends Infix

  def unparse(e: E): String = printable(e).print(false)
  def printable(exp: E): Printable
}

package spire.syntax.macros

import quoted._
import collection.immutable.NumericRange

import spire.syntax.fastFor.{RangeElem, RangeLike}

inline def fastForInline[R](init: R, test: R => Boolean, next: R => R, body: R => Unit): Unit =
  var index = init
  while (test(index))
  body(index)
  index = next(index)

def fastForRangeMacroGen[R <: RangeLike: Type](r: Expr[R], body: Expr[RangeElem[R] => Unit])(using
  quotes: Quotes
): Expr[Unit] =
  import quotes._
  import quotes.reflect._

  type RangeL = NumericRange[Long]

  (r, body) match
    case '{ $r: Range } -> '{ $body: (Int => Unit) }               => fastForRangeMacro(r, body)
    case '{ $r: NumericRange[Long] } -> '{ $body: (Long => Unit) } => fastForRangeMacroLong(r, body)
    case '{ $r } -> _                                              => report.error(s"Ineligible Range type ", r); '{}

end fastForRangeMacroGen

def fastForRangeMacroLong(r: Expr[NumericRange[Long]], body: Expr[Long => Unit])(using quotes: Quotes): Expr[Unit] =
  import quotes._
  import quotes.reflect.*

  def strideUpUntil(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    val body0 = $body
    while index < limit do
      ${ Expr.betaReduce(body) }(index)
      index += $stride
  }

  def strideUpTo(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val end = $untilExpr
    while index <= end do
      ${ Expr.betaReduce(body) }(index)
      index += $stride
  }

  def strideDownTo(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val end = $untilExpr
    while index >= end do
      ${ Expr.betaReduce(body) }(index)
      index -= $stride
  }

  def strideDownUntil(fromExpr: Expr[Long], untilExpr: Expr[Long], stride: Expr[Long]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while index > limit do
      ${ Expr.betaReduce(body) }(index)
      index -= $stride
  }

  r match
    case '{ ($i: Long) until $j } => strideUpUntil(i, j, Expr(1L))
    case '{ ($i: Long) to $j }    => strideUpTo(i, j, Expr(1L))
    case '{ ($i: Long) until $j by $step } =>
      step.asTerm match {
        case Literal(LongConstant(k)) if k > 0  => strideUpUntil(i, j, Expr(k))
        case Literal(LongConstant(k)) if k < 0  => strideDownUntil(i, j, Expr(-k))
        case Literal(LongConstant(k)) if k == 0 => report.error("zero stride", step); '{}
        case _ =>
          report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
          '{ val b = $body; $r.foreach(b) }
      }
    case '{ ($i: Long) to $j by $step } =>
      step.asTerm match {
        case Literal(LongConstant(k)) if k > 0  => strideUpTo(i, j, Expr(k))
        case Literal(LongConstant(k)) if k < 0  => strideDownTo(i, j, Expr(-k))
        case Literal(LongConstant(k)) if k == 0 => report.error("zero stride", step); '{}
        case _ =>
          report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
          '{ val b = $body; $r.foreach(b) }
      }

    case _ =>
      report.warning(s"defaulting to foreach, can not optimise range expression", r)
      '{ val b = $body; $r.foreach(b) }

end fastForRangeMacroLong

def fastForRangeMacro(r: Expr[Range], body: Expr[Int => Unit])(using quotes: Quotes): Expr[Unit] =
  import quotes._
  import quotes.reflect._

  def strideUpUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index < limit) {
      ${ Expr.betaReduce(body) }(index)
      index += $stride
    }
  }

  def strideUpTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val end = $untilExpr
    while (index <= end)
    ${ Expr.betaReduce(body) }(index)
    index += $stride
  }

  def strideDownTo(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val end = $untilExpr
    while (index >= end)
    ${ Expr.betaReduce(body) }(index)
    index -= $stride
  }

  def strideDownUntil(fromExpr: Expr[Int], untilExpr: Expr[Int], stride: Expr[Int]): Expr[Unit] = '{
    var index = $fromExpr
    val limit = $untilExpr
    while (index > limit)
    ${ Expr.betaReduce(body) }(index)
    index -= $stride
  }

  r match
    case '{ ($i: Int) until $j } => strideUpUntil(i, j, Expr(1))
    case '{ ($i: Int) to $j }    => strideUpTo(i, j, Expr(1))
    case '{ ($i: Int) until $j by $step } =>
      step.asTerm match {
        case Literal(IntConstant(k)) if k > 0  => strideUpUntil(i, j, Expr(k))
        case Literal(IntConstant(k)) if k < 0  => strideDownUntil(i, j, Expr(-k))
        case Literal(IntConstant(k)) if k == 0 => report.error("zero stride", step); '{}
        case _ =>
          report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
          '{ val b = $body; $r.foreach(b) }
      }
    case '{ ($i: Int) to $j by $step } =>
      step.asTerm match {
        case Literal(IntConstant(k)) if k > 0  => strideUpTo(i, j, Expr(k))
        case Literal(IntConstant(k)) if k < 0  => strideDownTo(i, j, Expr(-k))
        case Literal(IntConstant(k)) if k == 0 => report.error("zero stride", step); '{}
        case _ =>
          report.warning(s"defaulting to foreach, can not optimise non-constant step", step)
          '{ val b = $body; $r.foreach(b) }
      }
    case _ =>
      report.warning(s"defaulting to foreach, can not optimise range expression", r)
      '{ val b = $body; $r.foreach(b) }

end fastForRangeMacro

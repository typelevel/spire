/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire.syntax.macros

import scala.quoted.*
import scala.collection.immutable.NumericRange
import scala.PartialFunction.cond

import spire.syntax.fastFor.{RangeElem, RangeLike}

def fastForImpl[R: Type](init: Expr[R], test: Expr[R => Boolean], next: Expr[R => R], body: Expr[R => Unit])(using
  Quotes
): Expr[Unit] =
  import quotes.reflect.*

  def code(testRef: Expr[R => Boolean], nextRef: Expr[R => R], bodyRef: Expr[R => Unit]): Expr[Unit] = '{
    var index = $init
    while $testRef(index) do
      $bodyRef(index)
      index = $nextRef(index)
  }

  letFunc("test", test)(t => letFunc("next", next)(n => letFunc("body", body)(b => code(t, n, b))))
end fastForImpl

def fastForRangeMacroGen[R <: RangeLike: Type](r: Expr[R], body: Expr[RangeElem[R] => Unit])(using
  quotes: Quotes
): Expr[Unit] =
  import quotes.reflect.*

  r match
    case '{ $r: Range }              => RangeForImpl.ofInt(r, body.asExprOf[Int => Unit])
    case '{ $r: NumericRange[Long] } => RangeForImpl.ofLong(r, body.asExprOf[Long => Unit])
    case '{ $r }                     => report.error(s"Ineligible Range type ", r); '{}

end fastForRangeMacroGen

private object RangeForImpl:
  type Code[T] = Expr[T => Unit] => Expr[Unit]
  type Test[T] = (Expr[T], Expr[T]) => Expr[Boolean]

  def ofInt(r: Expr[Range], body: Expr[Int => Unit])(using Quotes): Expr[Unit] =
    val code: Code[Int] = r match
      case '{ ($i: Int) to $j }                              => loopCode(i, j, 1, (x, y) => '{ $x <= $y })
      case '{ ($i: Int) to $j by ${ Expr(k) } } if k > 0     => loopCode(i, j, k, (x, y) => '{ $x <= $y })
      case '{ ($i: Int) to $j by ${ Expr(k) } } if k < 0     => loopCode(i, j, k, (x, y) => '{ $x >= $y })
      case '{ ($i: Int) to $j by ${ Expr(k) } } if k == 0    => zeroStride(r)
      case '{ ($i: Int) until $j }                           => loopCode(i, j, 1, (x, y) => '{ $x < $y })
      case '{ ($i: Int) until $j by ${ Expr(k) } } if k > 0  => loopCode(i, j, k, (x, y) => '{ $x < $y })
      case '{ ($i: Int) until $j by ${ Expr(k) } } if k < 0  => loopCode(i, j, k, (x, y) => '{ $x > $y })
      case '{ ($i: Int) until $j by ${ Expr(k) } } if k == 0 => zeroStride(r)
      case _                                                 => deOpt(r, '{ $r.foreach($body) })

    letFunc("body", body)(code)
  end ofInt

  def ofLong(r: Expr[NumericRange[Long]], body: Expr[Long => Unit])(using quotes: Quotes): Expr[Unit] =
    val code: Code[Long] = r match
      case '{ ($i: Long) to $j }                              => loopCode(i, j, 1L, (x, y) => '{ $x <= $y })
      case '{ ($i: Long) to $j by ${ Expr(k) } } if k > 0     => loopCode(i, j, k, (x, y) => '{ $x <= $y })
      case '{ ($i: Long) to $j by ${ Expr(k) } } if k < 0     => loopCode(i, j, k, (x, y) => '{ $x >= $y })
      case '{ ($i: Long) to $j by ${ Expr(k) } } if k == 0    => zeroStride(r)
      case '{ ($i: Long) until $j }                           => loopCode(i, j, 1L, (x, y) => '{ $x < $y })
      case '{ ($i: Long) until $j by ${ Expr(k) } } if k > 0  => loopCode(i, j, k, (x, y) => '{ $x < $y })
      case '{ ($i: Long) until $j by ${ Expr(k) } } if k < 0  => loopCode(i, j, k, (x, y) => '{ $x > $y })
      case '{ ($i: Long) until $j by ${ Expr(k) } } if k == 0 => zeroStride(r)
      case _                                                  => deOpt(r, '{ $r.foreach($body) })

    letFunc("body", body)(code)

  end ofLong

  def loopCode[T: Type: ToExpr: CanLoop](i: Expr[T], j: Expr[T], s: T, test: Test[T])(using Quotes): Code[T] =
    body =>
      '{
        var index = $i
        val limit = $j
        while ${ test('index, 'limit) } do
          $body(index)
          index = ${ 'index.stepBy(Expr(s)) }
      }

  def zeroStride[T, R](orig: Expr[R])(using Quotes): Code[T] = _ =>
    import quotes.reflect.*
    report.error("zero stride", orig)
    '{}

  def deOpt[T, R](orig: Expr[R], foreach: Expr[Unit])(using Quotes): Code[T] = _ =>
    import quotes.reflect.*
    report.warning(s"defaulting to foreach, can not optimise range expression", orig)
    foreach

  trait CanLoop[T]:
    extension (x: Expr[T]) def stepBy(y: Expr[T])(using Quotes): Expr[T]

  object CanLoop:
    given CanLoop[Int] with
      extension (x: Expr[Int]) def stepBy(y: Expr[Int])(using Quotes): Expr[Int] = '{ $x + $y }

    given CanLoop[Long] with
      extension (x: Expr[Long]) def stepBy(y: Expr[Long])(using Quotes): Expr[Long] = '{ $x + $y }

end RangeForImpl

/**
 * Equivalent to `'{ val name: A => B = $rhs; ${in('name)} }`, except when `rhs` is a function literal, then equivalent
 * to `in(rhs)`.
 *
 * This allows inlined function arguments to perform side-effects only once before their first evaluation, while still
 * avoiding the creation of closures for function literal arguments.
 */
private def letFunc[A, B, C](using Quotes)(name: String, rhs: Expr[A => B])(in: Expr[A => B] => Expr[C]): Expr[C] =
  import quotes.reflect.*

  extension (t: Term) def unsafeAsExpr[A] = t.asExpr.asInstanceOf[Expr[A]] // cast without `quoted.Type[A]`

  def isFunctionLiteral[A, B](f: Expr[A => B]): Boolean = cond(f.asTerm.underlyingArgument) { case Lambda(_, _) =>
    true
  }

  def let[A, B](name: String, rhs: Expr[A])(in: Expr[A] => Expr[B])(using Quotes): Expr[B] =
    // Equivalent to `'{ val name = $rhs; ${in('name)} }`
    ValDef.let(Symbol.spliceOwner, name, rhs.asTerm)(ref => in(ref.unsafeAsExpr[A]).asTerm).unsafeAsExpr[B]

  if isFunctionLiteral(rhs) then in(Expr.betaReduce(rhs))
  else let(name, rhs)(in)

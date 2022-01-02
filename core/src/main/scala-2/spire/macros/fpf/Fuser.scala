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

package spire
package macros.fpf

import spire.macros.compat.{freshTermName, resetLocalAttrs, typeCheck, Context}
import spire.math.{FpFilter, FpFilterApprox, FpFilterExact}

private[spire] trait Fuser[C <: Context, A] {
  val c: C
  implicit def A: c.WeakTypeTag[A]

  import c.universe._

  private def Epsilon: Tree = q"2.220446049250313E-16"
  private def PositiveInfinity: Tree = q"_root_.java.lang.Double.POSITIVE_INFINITY"
  private def NegativeInfinity: Tree = q"_root_.java.lang.Double.NEGATIVE_INFINITY"
  private def max(a: Tree, b: Tree): Tree = q"_root_.java.lang.Math.max($a, $b)"
  private def abs(a: TermName): Tree = q"_root_.java.lang.Math.abs($a)"
  private def abs(a: Tree): Tree = q"_root_.java.lang.Math.abs($a)"
  private def sqrt(a: TermName): Tree = q"_root_.java.lang.Math.sqrt($a)"

  def intLit(n: Int): Tree = q"$n"

  case class Approx(apx: Tree, mes: Tree, ind: Either[Tree, Int], exact: Tree) {
    def expr: Tree = {
      val ind0: Tree = ind.fold(t => t, intLit)
      q"_root_.spire.math.FpFilter[$A]($apx, $mes, $ind0, $exact)"
    }

    def fused(stats0: List[Tree]): Fused = {
      val (apx0, mes0, ind0, exact0) = freshApproxNames()
      val indValDef = ind.fold(t => q"val $ind0 = $t" :: Nil, _ => Nil)
      val stats1 = List(q"val $apx0 = $apx", q"val $mes0 = $mes", q"def $exact0 = $exact") ++ indValDef
      Fused(stats0 ++ stats1, apx0, mes0, ind.left.map(_ => ind0), exact0)
    }
  }

  case class Fused(stats: List[Tree], apx: TermName, mes: TermName, ind: Either[TermName, Int], exact: TermName) {
    def approx: Approx = Approx(q"$apx", q"$mes", ind.left.map(ind0 => q"$ind0"), q"$exact")
    def expr: Tree = resetLocalAttrs(c)(Block(stats, approx.expr))
  }

  private def liftExact(exact: Tree): Fused = {
    val tmp = freshTermName(c)("fpf$tmp$")
    Approx(
      q"$tmp",
      abs(tmp),
      Right(0),
      q"_root_.spire.algebra.Field[$A].fromDouble($tmp)"
    ).fused(q"val $tmp = $exact.value" :: Nil)
  }

  private def liftApprox(approx: Tree): Fused = {
    val tmp = freshTermName(c)("fpf$tmp$")
    Approx(
      q"$tmp",
      abs(tmp),
      Right(1),
      q"$approx.exact"
    ).fused(q"val $tmp = _root_.spire.algebra.IsReal[$A].toDouble($approx.exact)" :: Nil)
  }

  private def extract(tree: Tree): Fused = resetLocalAttrs(c)(tree) match {
    case block @ Block(stats, expr) =>
      extract(expr) match {
        case Fused(Nil, apx, mes, ind, exact) =>
          Fused(stats, apx, mes, ind, exact)

        case bounded =>
          val tmp = freshTermName(c)("fpf$tmp$")
          val stats0 = stats :+ q"val $tmp = ${bounded.expr}"
          Approx(q"$tmp.apx", q"$tmp.mes", Left(q"$tmp.ind"), q"$tmp.exact").fused(stats0)
      }

    case q"$constr($apx, $mes, $ind, $exact)" =>
      termify(apx, mes, ind, exact)
        .map { case (apx, mes, ind, exact) =>
          Fused(Nil, apx, mes, ind, exact)
        }
        .getOrElse(Approx(apx, mes, Left(ind), exact).fused(Nil))

    case _ if typeCheck(c)(tree).tpe <:< c.weakTypeOf[FpFilterExact[A]] =>
      liftExact(tree)

    case _ if typeCheck(c)(tree).tpe <:< c.weakTypeOf[FpFilterApprox[A]] =>
      liftApprox(tree)

    case q"$lift($exact)($ev)" if isExactLift(tree) =>
      liftExact(exact)

    case q"$lift($approx)($ev)" if isApproxLift(tree) =>
      liftApprox(approx)

    case expr =>
      val tmp = freshTermName(c)("fpf$tmp$")
      val assign = q"val $tmp = $tree"
      Approx(q"$tmp.apx", q"$tmp.mes", Left(q"$tmp.ind"), q"$tmp.exact").fused(assign :: Nil)
  }

  // Returns true if `tree` is lifting an exact type tpe
  private def isExactLift(tree: Tree): Boolean = tree match {
    case q"$lift($exact)($ev)" =>
      (typeCheck(c)(tree).tpe <:< c.weakTypeOf[FpFilter[A]]) &&
      (typeCheck(c)(exact).tpe <:< c.weakTypeOf[FpFilterExact[A]])
    case _ => false
  }

  private def isApproxLift(tree: Tree): Boolean = tree match {
    case q"$lift($approx)($ev)" =>
      (typeCheck(c)(tree).tpe <:< c.weakTypeOf[FpFilter[A]]) &&
      (typeCheck(c)(approx).tpe <:< c.weakTypeOf[FpFilterApprox[A]])
    case _ => false
  }

  private def termify(apx: Tree,
                      mes: Tree,
                      ind: Tree,
                      exact: Tree
  ): Option[(TermName, TermName, Either[TermName, Int], TermName)] = {
    def t(tree: Tree): Option[TermName] = tree match {
      case Ident(name: TermName) => Some(name: TermName)
      case _                     => None
    }

    def l(tree: Tree): Option[Int] = tree match {
      case Literal(Constant(n: Int)) => Some(n)
      case _                         => None
    }

    val ind0 = t(ind).map(Left(_)).orElse(l(ind).map(Right(_)))

    for (a <- t(apx); b <- t(mes); c <- ind0; d <- t(exact)) yield {
      (a, b, c, d)
    }
  }

  private def freshApproxNames(): (TermName, TermName, TermName, TermName) = {
    val apx = freshTermName(c)("fpf$apx$")
    val mes = freshTermName(c)("fpf$mes$")
    val ind = freshTermName(c)("fpf$ind$")
    val exact = freshTermName(c)("fpf$exact$")
    (apx, mes, ind, exact)
  }

  private def zipInd(a: Either[Tree, Int],
                     b: Either[Tree, Int]
  )(f: (Tree, Tree) => Tree, g: (Int, Int) => Int): Either[Tree, Int] = {
    (a, b) match {
      case (Right(n), Right(m)) => Right(g(n, m))
      case (Right(n), Left(t))  => Left(f(intLit(n), t))
      case (Left(t), Right(n))  => Left(f(t, intLit(n)))
      case (Left(t), Left(u))   => Left(f(t, u))
    }
  }

  private def fuse2(lhs: Tree, rhs: Tree)(f: (Approx, Approx) => Approx): Fused = {
    val lfused = extract(lhs)
    val rfused = extract(rhs)
    f(lfused.approx, rfused.approx).fused(lfused.stats ++ rfused.stats)
  }

  private def resign(sub: Tree)(f: (TermName, TermName) => (Tree, Tree)): Fused = {
    val fused = extract(sub)
    val (apx, _, _, exact) = freshApproxNames()
    val (apx0, exact0) = f(fused.apx, fused.exact)
    val stats = fused.stats :+ q"val $apx = $apx0" :+ q"def $exact = $exact0"
    fused.copy(stats = stats, apx = apx, exact = exact)
  }

  def negate(sub: Tree)(ev: Tree): Fused =
    resign(sub) { (apx, exact) => (q"-$apx", q"$ev.negate($exact)") }

  def abs(sub: Tree, ev: Tree): Fused =
    resign(sub) { (apx, exact) => (abs(apx), q"$ev.abs($exact)") }

  def sqrt(tree: Tree)(ev: Tree): Fused = {
    val fused = extract(tree)
    val (apx, mes, ind, exact) = freshApproxNames()
    val indValDef = fused.ind.fold(n => q"val $ind = $n + 1" :: Nil, _ => Nil)
    val stats = List(
      q"val $apx = ${sqrt(fused.apx)}",
      q"""val $mes =
        if (${fused.apx} < 0) {
          ${sqrt(fused.mes)} * (1 << 26)
        } else {
          (${fused.mes} / ${fused.apx}) * $apx
        }
      """,
      q"def $exact = $ev.sqrt(${fused.exact})"
    ) ++ indValDef
    val ind0 = fused.ind.fold(_ => Left(ind), n => Right(n + 1))
    val result = Fused(fused.stats ++ stats, apx, mes, ind0, exact)
    result
  }

  // private def mix(a: Either[Tree, Int], b: Either[Tree, Int]): Either[(Tree, Tree), (Int, Int)] = {
  def plus(lhs: Tree, rhs: Tree)(ev: Tree): Fused = fuse2(lhs, rhs) {
    case (Approx(lapx, lmes, lind, lexact), Approx(rapx, rmes, rind, rexact)) =>
      val ind = zipInd(lind, rind)((l, r) => q"${max(l, r)} + 1", (l, r) => spire.math.max(l, r) + 1)
      Approx(q"$lapx + $rapx", q"$lmes + $rmes", ind, q"$ev.plus($lexact, $rexact)")
  }

  def minus(lhs: Tree, rhs: Tree)(ev: Tree): Fused = fuse2(lhs, rhs) {
    case (Approx(lapx, lmes, lind, lexact), Approx(rapx, rmes, rind, rexact)) =>
      val ind = zipInd(lind, rind)((l, r) => q"${max(l, r)} + 1", (l, r) => spire.math.max(l, r) + 1)
      Approx(q"$lapx - $rapx", q"$lmes + $rmes", ind, q"$ev.minus($lexact, $rexact)")
  }

  def times(lhs: Tree, rhs: Tree)(ev: Tree): Fused = fuse2(lhs, rhs) {
    case (Approx(lapx, lmes, lind, lexact), Approx(rapx, rmes, rind, rexact)) =>
      val ind = zipInd(lind, rind)((l, r) => q"$l + $r + 1", (l, r) => l + r + 1)
      Approx(q"$lapx * $rapx", q"$lmes * $rmes", ind, q"$ev.times($lexact, $rexact)")
  }

  def divide(lhs: Tree, rhs: Tree)(ev: Tree): Fused = fuse2(lhs, rhs) {
    case (Approx(lapx, lmes, lind, lexact), Approx(rapx, rmes, rind, rexact)) =>
      val tmp = freshTermName(c)("fpf$tmp$")
      val rindp1 = rind.fold(rind0 => q"$rind0 + 1", n => q"${intLit(n)} + 1")
      Approx(
        q"$lapx / $rapx",
        q"""
          val $tmp = ${abs(rapx)}
          (${abs(lapx)} / $tmp + ($lmes / $rmes)) / ($tmp / $rmes - $rindp1 * $Epsilon)
        """,
        zipInd(lind, rind)((l, _) => q"${max(l, rindp1)} + 1", (l, r) => spire.math.max(l, r + 1) + 1),
        q"$ev.div($lexact, $rexact)"
      )
  }

  def sign(tree: Tree)(signed: Tree): Tree = {
    val Fused(stats, apx, mes, ind, exact) = extract(tree)
    val err = freshTermName(c)("fpf$err$")
    val ind0 = ind.fold(name => q"$name", intLit)
    val block = Block(
      stats :+ q"val $err = $mes * $ind0 * $Epsilon",
      q"""
        if ($apx > $err && $apx < $PositiveInfinity) 1
        else if ($apx < -$err && $apx > $NegativeInfinity) -1
        else if ($err == 0D) 0
        else $signed.signum($exact)
      """
    )
    block
  }

  private def mkComp(t: Tree): Cmp => Tree = {
    case Cmp.Lt   => q"$t < 0"
    case Cmp.Gt   => q"$t > 0"
    case Cmp.LtEq => q"$t <= 0"
    case Cmp.GtEq => q"$t >= 0"
    case Cmp.Eq   => q"$t == 0"
  }

  def comp(lhs: Tree, rhs: Tree)(rng: Tree, signed: Tree)(cmp: Cmp): Tree = {
    val result = sign(minus(lhs, rhs)(rng).expr)(signed)
    mkComp(result)(cmp)
  }
}

private[spire] object Fuser {
  def apply[C <: Context, A: ctx.WeakTypeTag](ctx: C): Fuser[C, A] = new Fuser[C, A] {
    val c = ctx
    val A = c.weakTypeTag[A]
  }
}

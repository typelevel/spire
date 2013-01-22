package spire.math
import scala.{ specialized => spec }

/**************************************************************************
 * WARNING: This is an auto-generated file. Any changes will most likely  *
 * be overwritten the next time this file is regenerated.                 *
 **************************************************************************/

trait EqProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Eq[(A, B)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  def eqv(x0: (A, B), x1: (A, B)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2)
}
trait EqProduct3[A, B, C] extends Eq[(A, B, C)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  def eqv(x0: (A, B, C), x1: (A, B, C)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3)
}
trait EqProduct4[A, B, C, D] extends Eq[(A, B, C, D)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  def eqv(x0: (A, B, C, D), x1: (A, B, C, D)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4)
}
trait EqProduct5[A, B, C, D, E] extends Eq[(A, B, C, D, E)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  def eqv(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5)
}
trait EqProduct6[A, B, C, D, E, F] extends Eq[(A, B, C, D, E, F)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  def eqv(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6)
}
trait EqProduct7[A, B, C, D, E, F, G] extends Eq[(A, B, C, D, E, F, G)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  def eqv(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7)
}
trait EqProduct8[A, B, C, D, E, F, G, H] extends Eq[(A, B, C, D, E, F, G, H)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  def eqv(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8)
}
trait EqProduct9[A, B, C, D, E, F, G, H, I] extends Eq[(A, B, C, D, E, F, G, H, I)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  def eqv(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9)
}
trait EqProduct10[A, B, C, D, E, F, G, H, I, J] extends Eq[(A, B, C, D, E, F, G, H, I, J)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10)
}
trait EqProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Eq[(A, B, C, D, E, F, G, H, I, J, K)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11)
}
trait EqProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12)
}
trait EqProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13)
}
trait EqProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14)
}
trait EqProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15)
}
trait EqProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  implicit def structure16: Eq[P]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15) && structure16.eqv(x0._16, x1._16)
}
trait EqProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  implicit def structure16: Eq[P]
  implicit def structure17: Eq[Q]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15) && structure16.eqv(x0._16, x1._16) && structure17.eqv(x0._17, x1._17)
}
trait EqProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  implicit def structure16: Eq[P]
  implicit def structure17: Eq[Q]
  implicit def structure18: Eq[R]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15) && structure16.eqv(x0._16, x1._16) && structure17.eqv(x0._17, x1._17) && structure18.eqv(x0._18, x1._18)
}
trait EqProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  implicit def structure16: Eq[P]
  implicit def structure17: Eq[Q]
  implicit def structure18: Eq[R]
  implicit def structure19: Eq[S]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15) && structure16.eqv(x0._16, x1._16) && structure17.eqv(x0._17, x1._17) && structure18.eqv(x0._18, x1._18) && structure19.eqv(x0._19, x1._19)
}
trait EqProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  implicit def structure16: Eq[P]
  implicit def structure17: Eq[Q]
  implicit def structure18: Eq[R]
  implicit def structure19: Eq[S]
  implicit def structure20: Eq[T]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15) && structure16.eqv(x0._16, x1._16) && structure17.eqv(x0._17, x1._17) && structure18.eqv(x0._18, x1._18) && structure19.eqv(x0._19, x1._19) && structure20.eqv(x0._20, x1._20)
}
trait EqProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  implicit def structure16: Eq[P]
  implicit def structure17: Eq[Q]
  implicit def structure18: Eq[R]
  implicit def structure19: Eq[S]
  implicit def structure20: Eq[T]
  implicit def structure21: Eq[U]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15) && structure16.eqv(x0._16, x1._16) && structure17.eqv(x0._17, x1._17) && structure18.eqv(x0._18, x1._18) && structure19.eqv(x0._19, x1._19) && structure20.eqv(x0._20, x1._20) && structure21.eqv(x0._21, x1._21)
}
trait EqProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
  implicit def structure1: Eq[A]
  implicit def structure2: Eq[B]
  implicit def structure3: Eq[C]
  implicit def structure4: Eq[D]
  implicit def structure5: Eq[E]
  implicit def structure6: Eq[F]
  implicit def structure7: Eq[G]
  implicit def structure8: Eq[H]
  implicit def structure9: Eq[I]
  implicit def structure10: Eq[J]
  implicit def structure11: Eq[K]
  implicit def structure12: Eq[L]
  implicit def structure13: Eq[M]
  implicit def structure14: Eq[N]
  implicit def structure15: Eq[O]
  implicit def structure16: Eq[P]
  implicit def structure17: Eq[Q]
  implicit def structure18: Eq[R]
  implicit def structure19: Eq[S]
  implicit def structure20: Eq[T]
  implicit def structure21: Eq[U]
  implicit def structure22: Eq[V]
  def eqv(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) && structure3.eqv(x0._3, x1._3) && structure4.eqv(x0._4, x1._4) && structure5.eqv(x0._5, x1._5) && structure6.eqv(x0._6, x1._6) && structure7.eqv(x0._7, x1._7) && structure8.eqv(x0._8, x1._8) && structure9.eqv(x0._9, x1._9) && structure10.eqv(x0._10, x1._10) && structure11.eqv(x0._11, x1._11) && structure12.eqv(x0._12, x1._12) && structure13.eqv(x0._13, x1._13) && structure14.eqv(x0._14, x1._14) && structure15.eqv(x0._15, x1._15) && structure16.eqv(x0._16, x1._16) && structure17.eqv(x0._17, x1._17) && structure18.eqv(x0._18, x1._18) && structure19.eqv(x0._19, x1._19) && structure20.eqv(x0._20, x1._20) && structure21.eqv(x0._21, x1._21) && structure22.eqv(x0._22, x1._22)
}
trait EqProductImplicits {
  implicit def EqProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Eq[A], _structure2: Eq[B]): Eq[(A, B)] = {
    new EqProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def EqProduct3[A, B, C](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C]): Eq[(A, B, C)] = {
    new EqProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def EqProduct4[A, B, C, D](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D]): Eq[(A, B, C, D)] = {
    new EqProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def EqProduct5[A, B, C, D, E](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E]): Eq[(A, B, C, D, E)] = {
    new EqProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def EqProduct6[A, B, C, D, E, F](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F]): Eq[(A, B, C, D, E, F)] = {
    new EqProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def EqProduct7[A, B, C, D, E, F, G](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G]): Eq[(A, B, C, D, E, F, G)] = {
    new EqProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def EqProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H]): Eq[(A, B, C, D, E, F, G, H)] = {
    new EqProduct8[A, B, C, D, E, F, G, H] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
    }
  }
  implicit def EqProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I]): Eq[(A, B, C, D, E, F, G, H, I)] = {
    new EqProduct9[A, B, C, D, E, F, G, H, I] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
    }
  }
  implicit def EqProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J]): Eq[(A, B, C, D, E, F, G, H, I, J)] = {
    new EqProduct10[A, B, C, D, E, F, G, H, I, J] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
    }
  }
  implicit def EqProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K]): Eq[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new EqProduct11[A, B, C, D, E, F, G, H, I, J, K] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
    }
  }
  implicit def EqProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new EqProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
    }
  }
  implicit def EqProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new EqProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
    }
  }
  implicit def EqProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new EqProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
    }
  }
  implicit def EqProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new EqProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
    }
  }
  implicit def EqProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O], _structure16: Eq[P]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new EqProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
    }
  }
  implicit def EqProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O], _structure16: Eq[P], _structure17: Eq[Q]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new EqProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
    }
  }
  implicit def EqProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O], _structure16: Eq[P], _structure17: Eq[Q], _structure18: Eq[R]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new EqProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
    }
  }
  implicit def EqProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O], _structure16: Eq[P], _structure17: Eq[Q], _structure18: Eq[R], _structure19: Eq[S]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new EqProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
    }
  }
  implicit def EqProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O], _structure16: Eq[P], _structure17: Eq[Q], _structure18: Eq[R], _structure19: Eq[S], _structure20: Eq[T]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new EqProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
      val structure20 = _structure20
    }
  }
  implicit def EqProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O], _structure16: Eq[P], _structure17: Eq[Q], _structure18: Eq[R], _structure19: Eq[S], _structure20: Eq[T], _structure21: Eq[U]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new EqProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
      val structure20 = _structure20
      val structure21 = _structure21
    }
  }
  implicit def EqProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Eq[A], _structure2: Eq[B], _structure3: Eq[C], _structure4: Eq[D], _structure5: Eq[E], _structure6: Eq[F], _structure7: Eq[G], _structure8: Eq[H], _structure9: Eq[I], _structure10: Eq[J], _structure11: Eq[K], _structure12: Eq[L], _structure13: Eq[M], _structure14: Eq[N], _structure15: Eq[O], _structure16: Eq[P], _structure17: Eq[Q], _structure18: Eq[R], _structure19: Eq[S], _structure20: Eq[T], _structure21: Eq[U], _structure22: Eq[V]): Eq[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new EqProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
      val structure20 = _structure20
      val structure21 = _structure21
      val structure22 = _structure22
    }
  }
}
trait OrderProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Order[(A, B)] with EqProduct2[A, B] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  def compare(x0: (A, B), x1: (A, B)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        0
      }
    }
}
}
trait OrderProduct3[A, B, C] extends Order[(A, B, C)] with EqProduct3[A, B, C] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  def compare(x0: (A, B, C), x1: (A, B, C)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          0
        }
      }
    }
}
}
trait OrderProduct4[A, B, C, D] extends Order[(A, B, C, D)] with EqProduct4[A, B, C, D] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  def compare(x0: (A, B, C, D), x1: (A, B, C, D)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            0
          }
        }
      }
    }
}
}
trait OrderProduct5[A, B, C, D, E] extends Order[(A, B, C, D, E)] with EqProduct5[A, B, C, D, E] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  def compare(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              0
            }
          }
        }
      }
    }
}
}
trait OrderProduct6[A, B, C, D, E, F] extends Order[(A, B, C, D, E, F)] with EqProduct6[A, B, C, D, E, F] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  def compare(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                0
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct7[A, B, C, D, E, F, G] extends Order[(A, B, C, D, E, F, G)] with EqProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  def compare(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  0
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct8[A, B, C, D, E, F, G, H] extends Order[(A, B, C, D, E, F, G, H)] with EqProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  def compare(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    0
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct9[A, B, C, D, E, F, G, H, I] extends Order[(A, B, C, D, E, F, G, H, I)] with EqProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  def compare(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct10[A, B, C, D, E, F, G, H, I, J] extends Order[(A, B, C, D, E, F, G, H, I, J)] with EqProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        0
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Order[(A, B, C, D, E, F, G, H, I, J, K)] with EqProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L)] with EqProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            0
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with EqProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              0
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with EqProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with EqProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  0
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with EqProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  implicit def structure16: Order[P]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  cmp = structure16.compare(x0._16, x1._16)
                                  if (cmp != 0) cmp else {
                                    0
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with EqProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  implicit def structure16: Order[P]
  implicit def structure17: Order[Q]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  cmp = structure16.compare(x0._16, x1._16)
                                  if (cmp != 0) cmp else {
                                    cmp = structure17.compare(x0._17, x1._17)
                                    if (cmp != 0) cmp else {
                                      0
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with EqProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  implicit def structure16: Order[P]
  implicit def structure17: Order[Q]
  implicit def structure18: Order[R]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  cmp = structure16.compare(x0._16, x1._16)
                                  if (cmp != 0) cmp else {
                                    cmp = structure17.compare(x0._17, x1._17)
                                    if (cmp != 0) cmp else {
                                      cmp = structure18.compare(x0._18, x1._18)
                                      if (cmp != 0) cmp else {
                                        0
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with EqProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  implicit def structure16: Order[P]
  implicit def structure17: Order[Q]
  implicit def structure18: Order[R]
  implicit def structure19: Order[S]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  cmp = structure16.compare(x0._16, x1._16)
                                  if (cmp != 0) cmp else {
                                    cmp = structure17.compare(x0._17, x1._17)
                                    if (cmp != 0) cmp else {
                                      cmp = structure18.compare(x0._18, x1._18)
                                      if (cmp != 0) cmp else {
                                        cmp = structure19.compare(x0._19, x1._19)
                                        if (cmp != 0) cmp else {
                                          0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with EqProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  implicit def structure16: Order[P]
  implicit def structure17: Order[Q]
  implicit def structure18: Order[R]
  implicit def structure19: Order[S]
  implicit def structure20: Order[T]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  cmp = structure16.compare(x0._16, x1._16)
                                  if (cmp != 0) cmp else {
                                    cmp = structure17.compare(x0._17, x1._17)
                                    if (cmp != 0) cmp else {
                                      cmp = structure18.compare(x0._18, x1._18)
                                      if (cmp != 0) cmp else {
                                        cmp = structure19.compare(x0._19, x1._19)
                                        if (cmp != 0) cmp else {
                                          cmp = structure20.compare(x0._20, x1._20)
                                          if (cmp != 0) cmp else {
                                            0
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with EqProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  implicit def structure16: Order[P]
  implicit def structure17: Order[Q]
  implicit def structure18: Order[R]
  implicit def structure19: Order[S]
  implicit def structure20: Order[T]
  implicit def structure21: Order[U]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  cmp = structure16.compare(x0._16, x1._16)
                                  if (cmp != 0) cmp else {
                                    cmp = structure17.compare(x0._17, x1._17)
                                    if (cmp != 0) cmp else {
                                      cmp = structure18.compare(x0._18, x1._18)
                                      if (cmp != 0) cmp else {
                                        cmp = structure19.compare(x0._19, x1._19)
                                        if (cmp != 0) cmp else {
                                          cmp = structure20.compare(x0._20, x1._20)
                                          if (cmp != 0) cmp else {
                                            cmp = structure21.compare(x0._21, x1._21)
                                            if (cmp != 0) cmp else {
                                              0
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with EqProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  implicit def structure3: Order[C]
  implicit def structure4: Order[D]
  implicit def structure5: Order[E]
  implicit def structure6: Order[F]
  implicit def structure7: Order[G]
  implicit def structure8: Order[H]
  implicit def structure9: Order[I]
  implicit def structure10: Order[J]
  implicit def structure11: Order[K]
  implicit def structure12: Order[L]
  implicit def structure13: Order[M]
  implicit def structure14: Order[N]
  implicit def structure15: Order[O]
  implicit def structure16: Order[P]
  implicit def structure17: Order[Q]
  implicit def structure18: Order[R]
  implicit def structure19: Order[S]
  implicit def structure20: Order[T]
  implicit def structure21: Order[U]
  implicit def structure22: Order[V]
  def compare(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        cmp = structure3.compare(x0._3, x1._3)
        if (cmp != 0) cmp else {
          cmp = structure4.compare(x0._4, x1._4)
          if (cmp != 0) cmp else {
            cmp = structure5.compare(x0._5, x1._5)
            if (cmp != 0) cmp else {
              cmp = structure6.compare(x0._6, x1._6)
              if (cmp != 0) cmp else {
                cmp = structure7.compare(x0._7, x1._7)
                if (cmp != 0) cmp else {
                  cmp = structure8.compare(x0._8, x1._8)
                  if (cmp != 0) cmp else {
                    cmp = structure9.compare(x0._9, x1._9)
                    if (cmp != 0) cmp else {
                      cmp = structure10.compare(x0._10, x1._10)
                      if (cmp != 0) cmp else {
                        cmp = structure11.compare(x0._11, x1._11)
                        if (cmp != 0) cmp else {
                          cmp = structure12.compare(x0._12, x1._12)
                          if (cmp != 0) cmp else {
                            cmp = structure13.compare(x0._13, x1._13)
                            if (cmp != 0) cmp else {
                              cmp = structure14.compare(x0._14, x1._14)
                              if (cmp != 0) cmp else {
                                cmp = structure15.compare(x0._15, x1._15)
                                if (cmp != 0) cmp else {
                                  cmp = structure16.compare(x0._16, x1._16)
                                  if (cmp != 0) cmp else {
                                    cmp = structure17.compare(x0._17, x1._17)
                                    if (cmp != 0) cmp else {
                                      cmp = structure18.compare(x0._18, x1._18)
                                      if (cmp != 0) cmp else {
                                        cmp = structure19.compare(x0._19, x1._19)
                                        if (cmp != 0) cmp else {
                                          cmp = structure20.compare(x0._20, x1._20)
                                          if (cmp != 0) cmp else {
                                            cmp = structure21.compare(x0._21, x1._21)
                                            if (cmp != 0) cmp else {
                                              cmp = structure22.compare(x0._22, x1._22)
                                              if (cmp != 0) cmp else {
                                                0
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
}
trait OrderProductImplicits {
  implicit def OrderProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Order[A], _structure2: Order[B]): Order[(A, B)] = {
    new OrderProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def OrderProduct3[A, B, C](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C]): Order[(A, B, C)] = {
    new OrderProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def OrderProduct4[A, B, C, D](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D]): Order[(A, B, C, D)] = {
    new OrderProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def OrderProduct5[A, B, C, D, E](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E]): Order[(A, B, C, D, E)] = {
    new OrderProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def OrderProduct6[A, B, C, D, E, F](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F]): Order[(A, B, C, D, E, F)] = {
    new OrderProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def OrderProduct7[A, B, C, D, E, F, G](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G]): Order[(A, B, C, D, E, F, G)] = {
    new OrderProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def OrderProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H]): Order[(A, B, C, D, E, F, G, H)] = {
    new OrderProduct8[A, B, C, D, E, F, G, H] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
    }
  }
  implicit def OrderProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I]): Order[(A, B, C, D, E, F, G, H, I)] = {
    new OrderProduct9[A, B, C, D, E, F, G, H, I] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
    }
  }
  implicit def OrderProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J]): Order[(A, B, C, D, E, F, G, H, I, J)] = {
    new OrderProduct10[A, B, C, D, E, F, G, H, I, J] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
    }
  }
  implicit def OrderProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K]): Order[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new OrderProduct11[A, B, C, D, E, F, G, H, I, J, K] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
    }
  }
  implicit def OrderProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L]): Order[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new OrderProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
    }
  }
  implicit def OrderProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new OrderProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
    }
  }
  implicit def OrderProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new OrderProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
    }
  }
  implicit def OrderProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new OrderProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
    }
  }
  implicit def OrderProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O], _structure16: Order[P]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new OrderProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
    }
  }
  implicit def OrderProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O], _structure16: Order[P], _structure17: Order[Q]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new OrderProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
    }
  }
  implicit def OrderProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O], _structure16: Order[P], _structure17: Order[Q], _structure18: Order[R]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new OrderProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
    }
  }
  implicit def OrderProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O], _structure16: Order[P], _structure17: Order[Q], _structure18: Order[R], _structure19: Order[S]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new OrderProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
    }
  }
  implicit def OrderProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O], _structure16: Order[P], _structure17: Order[Q], _structure18: Order[R], _structure19: Order[S], _structure20: Order[T]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new OrderProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
      val structure20 = _structure20
    }
  }
  implicit def OrderProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O], _structure16: Order[P], _structure17: Order[Q], _structure18: Order[R], _structure19: Order[S], _structure20: Order[T], _structure21: Order[U]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new OrderProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
      val structure20 = _structure20
      val structure21 = _structure21
    }
  }
  implicit def OrderProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Order[A], _structure2: Order[B], _structure3: Order[C], _structure4: Order[D], _structure5: Order[E], _structure6: Order[F], _structure7: Order[G], _structure8: Order[H], _structure9: Order[I], _structure10: Order[J], _structure11: Order[K], _structure12: Order[L], _structure13: Order[M], _structure14: Order[N], _structure15: Order[O], _structure16: Order[P], _structure17: Order[Q], _structure18: Order[R], _structure19: Order[S], _structure20: Order[T], _structure21: Order[U], _structure22: Order[V]): Order[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new OrderProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
      val structure8 = _structure8
      val structure9 = _structure9
      val structure10 = _structure10
      val structure11 = _structure11
      val structure12 = _structure12
      val structure13 = _structure13
      val structure14 = _structure14
      val structure15 = _structure15
      val structure16 = _structure16
      val structure17 = _structure17
      val structure18 = _structure18
      val structure19 = _structure19
      val structure20 = _structure20
      val structure21 = _structure21
      val structure22 = _structure22
    }
  }
}
package spire.algebra
import scala.{ specialized => spec }

/**************************************************************************
 * WARNING: This is an auto-generated file. Any changes will most likely  *
 * be overwritten the next time this file is regenerated.                 *
 **************************************************************************/

trait SemigroupProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Semigroup[(A, B)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  def op(x0: (A, B), x1: (A, B)): (A, B) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2)) }
}
trait SemigroupProduct3[A, B, C] extends Semigroup[(A, B, C)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  def op(x0: (A, B, C), x1: (A, B, C)): (A, B, C) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3)) }
}
trait SemigroupProduct4[A, B, C, D] extends Semigroup[(A, B, C, D)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  def op(x0: (A, B, C, D), x1: (A, B, C, D)): (A, B, C, D) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4)) }
}
trait SemigroupProduct5[A, B, C, D, E] extends Semigroup[(A, B, C, D, E)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  def op(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5)) }
}
trait SemigroupProduct6[A, B, C, D, E, F] extends Semigroup[(A, B, C, D, E, F)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  def op(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6)) }
}
trait SemigroupProduct7[A, B, C, D, E, F, G] extends Semigroup[(A, B, C, D, E, F, G)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  def op(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7)) }
}
trait SemigroupProduct8[A, B, C, D, E, F, G, H] extends Semigroup[(A, B, C, D, E, F, G, H)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  def op(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8)) }
}
trait SemigroupProduct9[A, B, C, D, E, F, G, H, I] extends Semigroup[(A, B, C, D, E, F, G, H, I)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  def op(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9)) }
}
trait SemigroupProduct10[A, B, C, D, E, F, G, H, I, J] extends Semigroup[(A, B, C, D, E, F, G, H, I, J)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  def op(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10)) }
}
trait SemigroupProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11)) }
}
trait SemigroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12)) }
}
trait SemigroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13)) }
}
trait SemigroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14)) }
}
trait SemigroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15)) }
}
trait SemigroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  implicit def structure16: Semigroup[P]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15), structure16.op(x0._16, x1._16)) }
}
trait SemigroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  implicit def structure16: Semigroup[P]
  implicit def structure17: Semigroup[Q]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15), structure16.op(x0._16, x1._16), structure17.op(x0._17, x1._17)) }
}
trait SemigroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  implicit def structure16: Semigroup[P]
  implicit def structure17: Semigroup[Q]
  implicit def structure18: Semigroup[R]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15), structure16.op(x0._16, x1._16), structure17.op(x0._17, x1._17), structure18.op(x0._18, x1._18)) }
}
trait SemigroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  implicit def structure16: Semigroup[P]
  implicit def structure17: Semigroup[Q]
  implicit def structure18: Semigroup[R]
  implicit def structure19: Semigroup[S]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15), structure16.op(x0._16, x1._16), structure17.op(x0._17, x1._17), structure18.op(x0._18, x1._18), structure19.op(x0._19, x1._19)) }
}
trait SemigroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  implicit def structure16: Semigroup[P]
  implicit def structure17: Semigroup[Q]
  implicit def structure18: Semigroup[R]
  implicit def structure19: Semigroup[S]
  implicit def structure20: Semigroup[T]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15), structure16.op(x0._16, x1._16), structure17.op(x0._17, x1._17), structure18.op(x0._18, x1._18), structure19.op(x0._19, x1._19), structure20.op(x0._20, x1._20)) }
}
trait SemigroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  implicit def structure16: Semigroup[P]
  implicit def structure17: Semigroup[Q]
  implicit def structure18: Semigroup[R]
  implicit def structure19: Semigroup[S]
  implicit def structure20: Semigroup[T]
  implicit def structure21: Semigroup[U]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15), structure16.op(x0._16, x1._16), structure17.op(x0._17, x1._17), structure18.op(x0._18, x1._18), structure19.op(x0._19, x1._19), structure20.op(x0._20, x1._20), structure21.op(x0._21, x1._21)) }
}
trait SemigroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
  implicit def structure1: Semigroup[A]
  implicit def structure2: Semigroup[B]
  implicit def structure3: Semigroup[C]
  implicit def structure4: Semigroup[D]
  implicit def structure5: Semigroup[E]
  implicit def structure6: Semigroup[F]
  implicit def structure7: Semigroup[G]
  implicit def structure8: Semigroup[H]
  implicit def structure9: Semigroup[I]
  implicit def structure10: Semigroup[J]
  implicit def structure11: Semigroup[K]
  implicit def structure12: Semigroup[L]
  implicit def structure13: Semigroup[M]
  implicit def structure14: Semigroup[N]
  implicit def structure15: Semigroup[O]
  implicit def structure16: Semigroup[P]
  implicit def structure17: Semigroup[Q]
  implicit def structure18: Semigroup[R]
  implicit def structure19: Semigroup[S]
  implicit def structure20: Semigroup[T]
  implicit def structure21: Semigroup[U]
  implicit def structure22: Semigroup[V]
  def op(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.op(x0._1, x1._1), structure2.op(x0._2, x1._2), structure3.op(x0._3, x1._3), structure4.op(x0._4, x1._4), structure5.op(x0._5, x1._5), structure6.op(x0._6, x1._6), structure7.op(x0._7, x1._7), structure8.op(x0._8, x1._8), structure9.op(x0._9, x1._9), structure10.op(x0._10, x1._10), structure11.op(x0._11, x1._11), structure12.op(x0._12, x1._12), structure13.op(x0._13, x1._13), structure14.op(x0._14, x1._14), structure15.op(x0._15, x1._15), structure16.op(x0._16, x1._16), structure17.op(x0._17, x1._17), structure18.op(x0._18, x1._18), structure19.op(x0._19, x1._19), structure20.op(x0._20, x1._20), structure21.op(x0._21, x1._21), structure22.op(x0._22, x1._22)) }
}
trait SemigroupProductImplicits {
  implicit def SemigroupProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Semigroup[A], _structure2: Semigroup[B]): Semigroup[(A, B)] = {
    new SemigroupProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def SemigroupProduct3[A, B, C](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C]): Semigroup[(A, B, C)] = {
    new SemigroupProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def SemigroupProduct4[A, B, C, D](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D]): Semigroup[(A, B, C, D)] = {
    new SemigroupProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def SemigroupProduct5[A, B, C, D, E](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E]): Semigroup[(A, B, C, D, E)] = {
    new SemigroupProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def SemigroupProduct6[A, B, C, D, E, F](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F]): Semigroup[(A, B, C, D, E, F)] = {
    new SemigroupProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def SemigroupProduct7[A, B, C, D, E, F, G](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G]): Semigroup[(A, B, C, D, E, F, G)] = {
    new SemigroupProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def SemigroupProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H]): Semigroup[(A, B, C, D, E, F, G, H)] = {
    new SemigroupProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def SemigroupProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I]): Semigroup[(A, B, C, D, E, F, G, H, I)] = {
    new SemigroupProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def SemigroupProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J]): Semigroup[(A, B, C, D, E, F, G, H, I, J)] = {
    new SemigroupProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def SemigroupProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new SemigroupProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def SemigroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new SemigroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def SemigroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new SemigroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def SemigroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new SemigroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def SemigroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new SemigroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def SemigroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O], _structure16: Semigroup[P]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new SemigroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def SemigroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O], _structure16: Semigroup[P], _structure17: Semigroup[Q]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new SemigroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def SemigroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O], _structure16: Semigroup[P], _structure17: Semigroup[Q], _structure18: Semigroup[R]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new SemigroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def SemigroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O], _structure16: Semigroup[P], _structure17: Semigroup[Q], _structure18: Semigroup[R], _structure19: Semigroup[S]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new SemigroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def SemigroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O], _structure16: Semigroup[P], _structure17: Semigroup[Q], _structure18: Semigroup[R], _structure19: Semigroup[S], _structure20: Semigroup[T]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new SemigroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def SemigroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O], _structure16: Semigroup[P], _structure17: Semigroup[Q], _structure18: Semigroup[R], _structure19: Semigroup[S], _structure20: Semigroup[T], _structure21: Semigroup[U]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new SemigroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def SemigroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Semigroup[A], _structure2: Semigroup[B], _structure3: Semigroup[C], _structure4: Semigroup[D], _structure5: Semigroup[E], _structure6: Semigroup[F], _structure7: Semigroup[G], _structure8: Semigroup[H], _structure9: Semigroup[I], _structure10: Semigroup[J], _structure11: Semigroup[K], _structure12: Semigroup[L], _structure13: Semigroup[M], _structure14: Semigroup[N], _structure15: Semigroup[O], _structure16: Semigroup[P], _structure17: Semigroup[Q], _structure18: Semigroup[R], _structure19: Semigroup[S], _structure20: Semigroup[T], _structure21: Semigroup[U], _structure22: Semigroup[V]): Semigroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new SemigroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait MonoidProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Monoid[(A, B)] with SemigroupProduct2[A, B] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  def id: (A, B) = (structure1.id, structure2.id)
}
trait MonoidProduct3[A, B, C] extends Monoid[(A, B, C)] with SemigroupProduct3[A, B, C] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  def id: (A, B, C) = (structure1.id, structure2.id, structure3.id)
}
trait MonoidProduct4[A, B, C, D] extends Monoid[(A, B, C, D)] with SemigroupProduct4[A, B, C, D] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  def id: (A, B, C, D) = (structure1.id, structure2.id, structure3.id, structure4.id)
}
trait MonoidProduct5[A, B, C, D, E] extends Monoid[(A, B, C, D, E)] with SemigroupProduct5[A, B, C, D, E] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  def id: (A, B, C, D, E) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id)
}
trait MonoidProduct6[A, B, C, D, E, F] extends Monoid[(A, B, C, D, E, F)] with SemigroupProduct6[A, B, C, D, E, F] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  def id: (A, B, C, D, E, F) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id)
}
trait MonoidProduct7[A, B, C, D, E, F, G] extends Monoid[(A, B, C, D, E, F, G)] with SemigroupProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  def id: (A, B, C, D, E, F, G) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id)
}
trait MonoidProduct8[A, B, C, D, E, F, G, H] extends Monoid[(A, B, C, D, E, F, G, H)] with SemigroupProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  def id: (A, B, C, D, E, F, G, H) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id)
}
trait MonoidProduct9[A, B, C, D, E, F, G, H, I] extends Monoid[(A, B, C, D, E, F, G, H, I)] with SemigroupProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  def id: (A, B, C, D, E, F, G, H, I) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id)
}
trait MonoidProduct10[A, B, C, D, E, F, G, H, I, J] extends Monoid[(A, B, C, D, E, F, G, H, I, J)] with SemigroupProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  def id: (A, B, C, D, E, F, G, H, I, J) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id)
}
trait MonoidProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K)] with SemigroupProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  def id: (A, B, C, D, E, F, G, H, I, J, K) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id)
}
trait MonoidProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L)] with SemigroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id)
}
trait MonoidProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with SemigroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id)
}
trait MonoidProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with SemigroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id)
}
trait MonoidProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with SemigroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id)
}
trait MonoidProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with SemigroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  implicit def structure16: Monoid[P]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id, structure16.id)
}
trait MonoidProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with SemigroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  implicit def structure16: Monoid[P]
  implicit def structure17: Monoid[Q]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id, structure16.id, structure17.id)
}
trait MonoidProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with SemigroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  implicit def structure16: Monoid[P]
  implicit def structure17: Monoid[Q]
  implicit def structure18: Monoid[R]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id, structure16.id, structure17.id, structure18.id)
}
trait MonoidProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with SemigroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  implicit def structure16: Monoid[P]
  implicit def structure17: Monoid[Q]
  implicit def structure18: Monoid[R]
  implicit def structure19: Monoid[S]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id, structure16.id, structure17.id, structure18.id, structure19.id)
}
trait MonoidProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with SemigroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  implicit def structure16: Monoid[P]
  implicit def structure17: Monoid[Q]
  implicit def structure18: Monoid[R]
  implicit def structure19: Monoid[S]
  implicit def structure20: Monoid[T]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id, structure16.id, structure17.id, structure18.id, structure19.id, structure20.id)
}
trait MonoidProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with SemigroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  implicit def structure16: Monoid[P]
  implicit def structure17: Monoid[Q]
  implicit def structure18: Monoid[R]
  implicit def structure19: Monoid[S]
  implicit def structure20: Monoid[T]
  implicit def structure21: Monoid[U]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id, structure16.id, structure17.id, structure18.id, structure19.id, structure20.id, structure21.id)
}
trait MonoidProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with SemigroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: Monoid[A]
  implicit def structure2: Monoid[B]
  implicit def structure3: Monoid[C]
  implicit def structure4: Monoid[D]
  implicit def structure5: Monoid[E]
  implicit def structure6: Monoid[F]
  implicit def structure7: Monoid[G]
  implicit def structure8: Monoid[H]
  implicit def structure9: Monoid[I]
  implicit def structure10: Monoid[J]
  implicit def structure11: Monoid[K]
  implicit def structure12: Monoid[L]
  implicit def structure13: Monoid[M]
  implicit def structure14: Monoid[N]
  implicit def structure15: Monoid[O]
  implicit def structure16: Monoid[P]
  implicit def structure17: Monoid[Q]
  implicit def structure18: Monoid[R]
  implicit def structure19: Monoid[S]
  implicit def structure20: Monoid[T]
  implicit def structure21: Monoid[U]
  implicit def structure22: Monoid[V]
  def id: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = (structure1.id, structure2.id, structure3.id, structure4.id, structure5.id, structure6.id, structure7.id, structure8.id, structure9.id, structure10.id, structure11.id, structure12.id, structure13.id, structure14.id, structure15.id, structure16.id, structure17.id, structure18.id, structure19.id, structure20.id, structure21.id, structure22.id)
}
trait MonoidProductImplicits {
  implicit def MonoidProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Monoid[A], _structure2: Monoid[B]): Monoid[(A, B)] = {
    new MonoidProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def MonoidProduct3[A, B, C](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C]): Monoid[(A, B, C)] = {
    new MonoidProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def MonoidProduct4[A, B, C, D](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D]): Monoid[(A, B, C, D)] = {
    new MonoidProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def MonoidProduct5[A, B, C, D, E](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E]): Monoid[(A, B, C, D, E)] = {
    new MonoidProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def MonoidProduct6[A, B, C, D, E, F](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F]): Monoid[(A, B, C, D, E, F)] = {
    new MonoidProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def MonoidProduct7[A, B, C, D, E, F, G](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G]): Monoid[(A, B, C, D, E, F, G)] = {
    new MonoidProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def MonoidProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H]): Monoid[(A, B, C, D, E, F, G, H)] = {
    new MonoidProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def MonoidProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I]): Monoid[(A, B, C, D, E, F, G, H, I)] = {
    new MonoidProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def MonoidProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J]): Monoid[(A, B, C, D, E, F, G, H, I, J)] = {
    new MonoidProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def MonoidProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K]): Monoid[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new MonoidProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def MonoidProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new MonoidProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def MonoidProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new MonoidProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def MonoidProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new MonoidProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def MonoidProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new MonoidProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def MonoidProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O], _structure16: Monoid[P]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new MonoidProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def MonoidProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O], _structure16: Monoid[P], _structure17: Monoid[Q]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new MonoidProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def MonoidProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O], _structure16: Monoid[P], _structure17: Monoid[Q], _structure18: Monoid[R]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new MonoidProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def MonoidProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O], _structure16: Monoid[P], _structure17: Monoid[Q], _structure18: Monoid[R], _structure19: Monoid[S]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new MonoidProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def MonoidProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O], _structure16: Monoid[P], _structure17: Monoid[Q], _structure18: Monoid[R], _structure19: Monoid[S], _structure20: Monoid[T]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new MonoidProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def MonoidProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O], _structure16: Monoid[P], _structure17: Monoid[Q], _structure18: Monoid[R], _structure19: Monoid[S], _structure20: Monoid[T], _structure21: Monoid[U]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new MonoidProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def MonoidProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Monoid[A], _structure2: Monoid[B], _structure3: Monoid[C], _structure4: Monoid[D], _structure5: Monoid[E], _structure6: Monoid[F], _structure7: Monoid[G], _structure8: Monoid[H], _structure9: Monoid[I], _structure10: Monoid[J], _structure11: Monoid[K], _structure12: Monoid[L], _structure13: Monoid[M], _structure14: Monoid[N], _structure15: Monoid[O], _structure16: Monoid[P], _structure17: Monoid[Q], _structure18: Monoid[R], _structure19: Monoid[S], _structure20: Monoid[T], _structure21: Monoid[U], _structure22: Monoid[V]): Monoid[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new MonoidProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait GroupProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Group[(A, B)] with MonoidProduct2[A, B] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  def inverse(x0: (A, B)): (A, B) = { (structure1.inverse(x0._1), structure2.inverse(x0._2)) }
}
trait GroupProduct3[A, B, C] extends Group[(A, B, C)] with MonoidProduct3[A, B, C] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  def inverse(x0: (A, B, C)): (A, B, C) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3)) }
}
trait GroupProduct4[A, B, C, D] extends Group[(A, B, C, D)] with MonoidProduct4[A, B, C, D] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  def inverse(x0: (A, B, C, D)): (A, B, C, D) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4)) }
}
trait GroupProduct5[A, B, C, D, E] extends Group[(A, B, C, D, E)] with MonoidProduct5[A, B, C, D, E] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  def inverse(x0: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5)) }
}
trait GroupProduct6[A, B, C, D, E, F] extends Group[(A, B, C, D, E, F)] with MonoidProduct6[A, B, C, D, E, F] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  def inverse(x0: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6)) }
}
trait GroupProduct7[A, B, C, D, E, F, G] extends Group[(A, B, C, D, E, F, G)] with MonoidProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  def inverse(x0: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7)) }
}
trait GroupProduct8[A, B, C, D, E, F, G, H] extends Group[(A, B, C, D, E, F, G, H)] with MonoidProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  def inverse(x0: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8)) }
}
trait GroupProduct9[A, B, C, D, E, F, G, H, I] extends Group[(A, B, C, D, E, F, G, H, I)] with MonoidProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  def inverse(x0: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9)) }
}
trait GroupProduct10[A, B, C, D, E, F, G, H, I, J] extends Group[(A, B, C, D, E, F, G, H, I, J)] with MonoidProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10)) }
}
trait GroupProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Group[(A, B, C, D, E, F, G, H, I, J, K)] with MonoidProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11)) }
}
trait GroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L)] with MonoidProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12)) }
}
trait GroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with MonoidProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13)) }
}
trait GroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with MonoidProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14)) }
}
trait GroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with MonoidProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15)) }
}
trait GroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with MonoidProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  implicit def structure16: Group[P]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15), structure16.inverse(x0._16)) }
}
trait GroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with MonoidProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  implicit def structure16: Group[P]
  implicit def structure17: Group[Q]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15), structure16.inverse(x0._16), structure17.inverse(x0._17)) }
}
trait GroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with MonoidProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  implicit def structure16: Group[P]
  implicit def structure17: Group[Q]
  implicit def structure18: Group[R]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15), structure16.inverse(x0._16), structure17.inverse(x0._17), structure18.inverse(x0._18)) }
}
trait GroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with MonoidProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  implicit def structure16: Group[P]
  implicit def structure17: Group[Q]
  implicit def structure18: Group[R]
  implicit def structure19: Group[S]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15), structure16.inverse(x0._16), structure17.inverse(x0._17), structure18.inverse(x0._18), structure19.inverse(x0._19)) }
}
trait GroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with MonoidProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  implicit def structure16: Group[P]
  implicit def structure17: Group[Q]
  implicit def structure18: Group[R]
  implicit def structure19: Group[S]
  implicit def structure20: Group[T]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15), structure16.inverse(x0._16), structure17.inverse(x0._17), structure18.inverse(x0._18), structure19.inverse(x0._19), structure20.inverse(x0._20)) }
}
trait GroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with MonoidProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  implicit def structure16: Group[P]
  implicit def structure17: Group[Q]
  implicit def structure18: Group[R]
  implicit def structure19: Group[S]
  implicit def structure20: Group[T]
  implicit def structure21: Group[U]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15), structure16.inverse(x0._16), structure17.inverse(x0._17), structure18.inverse(x0._18), structure19.inverse(x0._19), structure20.inverse(x0._20), structure21.inverse(x0._21)) }
}
trait GroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with MonoidProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: Group[A]
  implicit def structure2: Group[B]
  implicit def structure3: Group[C]
  implicit def structure4: Group[D]
  implicit def structure5: Group[E]
  implicit def structure6: Group[F]
  implicit def structure7: Group[G]
  implicit def structure8: Group[H]
  implicit def structure9: Group[I]
  implicit def structure10: Group[J]
  implicit def structure11: Group[K]
  implicit def structure12: Group[L]
  implicit def structure13: Group[M]
  implicit def structure14: Group[N]
  implicit def structure15: Group[O]
  implicit def structure16: Group[P]
  implicit def structure17: Group[Q]
  implicit def structure18: Group[R]
  implicit def structure19: Group[S]
  implicit def structure20: Group[T]
  implicit def structure21: Group[U]
  implicit def structure22: Group[V]
  def inverse(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.inverse(x0._1), structure2.inverse(x0._2), structure3.inverse(x0._3), structure4.inverse(x0._4), structure5.inverse(x0._5), structure6.inverse(x0._6), structure7.inverse(x0._7), structure8.inverse(x0._8), structure9.inverse(x0._9), structure10.inverse(x0._10), structure11.inverse(x0._11), structure12.inverse(x0._12), structure13.inverse(x0._13), structure14.inverse(x0._14), structure15.inverse(x0._15), structure16.inverse(x0._16), structure17.inverse(x0._17), structure18.inverse(x0._18), structure19.inverse(x0._19), structure20.inverse(x0._20), structure21.inverse(x0._21), structure22.inverse(x0._22)) }
}
trait GroupProductImplicits {
  implicit def GroupProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Group[A], _structure2: Group[B]): Group[(A, B)] = {
    new GroupProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def GroupProduct3[A, B, C](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C]): Group[(A, B, C)] = {
    new GroupProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def GroupProduct4[A, B, C, D](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D]): Group[(A, B, C, D)] = {
    new GroupProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def GroupProduct5[A, B, C, D, E](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E]): Group[(A, B, C, D, E)] = {
    new GroupProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def GroupProduct6[A, B, C, D, E, F](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F]): Group[(A, B, C, D, E, F)] = {
    new GroupProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def GroupProduct7[A, B, C, D, E, F, G](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G]): Group[(A, B, C, D, E, F, G)] = {
    new GroupProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def GroupProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H]): Group[(A, B, C, D, E, F, G, H)] = {
    new GroupProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def GroupProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I]): Group[(A, B, C, D, E, F, G, H, I)] = {
    new GroupProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def GroupProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J]): Group[(A, B, C, D, E, F, G, H, I, J)] = {
    new GroupProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def GroupProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K]): Group[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new GroupProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def GroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L]): Group[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new GroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def GroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new GroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def GroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new GroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def GroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new GroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def GroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O], _structure16: Group[P]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new GroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def GroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O], _structure16: Group[P], _structure17: Group[Q]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new GroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def GroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O], _structure16: Group[P], _structure17: Group[Q], _structure18: Group[R]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new GroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def GroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O], _structure16: Group[P], _structure17: Group[Q], _structure18: Group[R], _structure19: Group[S]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new GroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def GroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O], _structure16: Group[P], _structure17: Group[Q], _structure18: Group[R], _structure19: Group[S], _structure20: Group[T]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new GroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def GroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O], _structure16: Group[P], _structure17: Group[Q], _structure18: Group[R], _structure19: Group[S], _structure20: Group[T], _structure21: Group[U]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new GroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def GroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Group[A], _structure2: Group[B], _structure3: Group[C], _structure4: Group[D], _structure5: Group[E], _structure6: Group[F], _structure7: Group[G], _structure8: Group[H], _structure9: Group[I], _structure10: Group[J], _structure11: Group[K], _structure12: Group[L], _structure13: Group[M], _structure14: Group[N], _structure15: Group[O], _structure16: Group[P], _structure17: Group[Q], _structure18: Group[R], _structure19: Group[S], _structure20: Group[T], _structure21: Group[U], _structure22: Group[V]): Group[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new GroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait AbGroupProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends AbGroup[(A, B)] with GroupProduct2[A, B] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
}
trait AbGroupProduct3[A, B, C] extends AbGroup[(A, B, C)] with GroupProduct3[A, B, C] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
}
trait AbGroupProduct4[A, B, C, D] extends AbGroup[(A, B, C, D)] with GroupProduct4[A, B, C, D] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
}
trait AbGroupProduct5[A, B, C, D, E] extends AbGroup[(A, B, C, D, E)] with GroupProduct5[A, B, C, D, E] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
}
trait AbGroupProduct6[A, B, C, D, E, F] extends AbGroup[(A, B, C, D, E, F)] with GroupProduct6[A, B, C, D, E, F] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
}
trait AbGroupProduct7[A, B, C, D, E, F, G] extends AbGroup[(A, B, C, D, E, F, G)] with GroupProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
}
trait AbGroupProduct8[A, B, C, D, E, F, G, H] extends AbGroup[(A, B, C, D, E, F, G, H)] with GroupProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
}
trait AbGroupProduct9[A, B, C, D, E, F, G, H, I] extends AbGroup[(A, B, C, D, E, F, G, H, I)] with GroupProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
}
trait AbGroupProduct10[A, B, C, D, E, F, G, H, I, J] extends AbGroup[(A, B, C, D, E, F, G, H, I, J)] with GroupProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
}
trait AbGroupProduct11[A, B, C, D, E, F, G, H, I, J, K] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K)] with GroupProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
}
trait AbGroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L)] with GroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
}
trait AbGroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with GroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
}
trait AbGroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with GroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
}
trait AbGroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with GroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
}
trait AbGroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with GroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
  implicit def structure16: AbGroup[P]
}
trait AbGroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with GroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
  implicit def structure16: AbGroup[P]
  implicit def structure17: AbGroup[Q]
}
trait AbGroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with GroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
  implicit def structure16: AbGroup[P]
  implicit def structure17: AbGroup[Q]
  implicit def structure18: AbGroup[R]
}
trait AbGroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with GroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
  implicit def structure16: AbGroup[P]
  implicit def structure17: AbGroup[Q]
  implicit def structure18: AbGroup[R]
  implicit def structure19: AbGroup[S]
}
trait AbGroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with GroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
  implicit def structure16: AbGroup[P]
  implicit def structure17: AbGroup[Q]
  implicit def structure18: AbGroup[R]
  implicit def structure19: AbGroup[S]
  implicit def structure20: AbGroup[T]
}
trait AbGroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with GroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
  implicit def structure16: AbGroup[P]
  implicit def structure17: AbGroup[Q]
  implicit def structure18: AbGroup[R]
  implicit def structure19: AbGroup[S]
  implicit def structure20: AbGroup[T]
  implicit def structure21: AbGroup[U]
}
trait AbGroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with GroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: AbGroup[A]
  implicit def structure2: AbGroup[B]
  implicit def structure3: AbGroup[C]
  implicit def structure4: AbGroup[D]
  implicit def structure5: AbGroup[E]
  implicit def structure6: AbGroup[F]
  implicit def structure7: AbGroup[G]
  implicit def structure8: AbGroup[H]
  implicit def structure9: AbGroup[I]
  implicit def structure10: AbGroup[J]
  implicit def structure11: AbGroup[K]
  implicit def structure12: AbGroup[L]
  implicit def structure13: AbGroup[M]
  implicit def structure14: AbGroup[N]
  implicit def structure15: AbGroup[O]
  implicit def structure16: AbGroup[P]
  implicit def structure17: AbGroup[Q]
  implicit def structure18: AbGroup[R]
  implicit def structure19: AbGroup[S]
  implicit def structure20: AbGroup[T]
  implicit def structure21: AbGroup[U]
  implicit def structure22: AbGroup[V]
}
trait AbGroupProductImplicits {
  implicit def AbGroupProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: AbGroup[A], _structure2: AbGroup[B]): AbGroup[(A, B)] = {
    new AbGroupProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def AbGroupProduct3[A, B, C](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C]): AbGroup[(A, B, C)] = {
    new AbGroupProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def AbGroupProduct4[A, B, C, D](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D]): AbGroup[(A, B, C, D)] = {
    new AbGroupProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def AbGroupProduct5[A, B, C, D, E](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E]): AbGroup[(A, B, C, D, E)] = {
    new AbGroupProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def AbGroupProduct6[A, B, C, D, E, F](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F]): AbGroup[(A, B, C, D, E, F)] = {
    new AbGroupProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def AbGroupProduct7[A, B, C, D, E, F, G](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G]): AbGroup[(A, B, C, D, E, F, G)] = {
    new AbGroupProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def AbGroupProduct8[A, B, C, D, E, F, G, H](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H]): AbGroup[(A, B, C, D, E, F, G, H)] = {
    new AbGroupProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def AbGroupProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I]): AbGroup[(A, B, C, D, E, F, G, H, I)] = {
    new AbGroupProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def AbGroupProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J]): AbGroup[(A, B, C, D, E, F, G, H, I, J)] = {
    new AbGroupProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def AbGroupProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new AbGroupProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def AbGroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new AbGroupProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def AbGroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new AbGroupProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def AbGroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new AbGroupProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def AbGroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new AbGroupProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def AbGroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O], _structure16: AbGroup[P]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new AbGroupProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def AbGroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O], _structure16: AbGroup[P], _structure17: AbGroup[Q]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new AbGroupProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def AbGroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O], _structure16: AbGroup[P], _structure17: AbGroup[Q], _structure18: AbGroup[R]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new AbGroupProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def AbGroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O], _structure16: AbGroup[P], _structure17: AbGroup[Q], _structure18: AbGroup[R], _structure19: AbGroup[S]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new AbGroupProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def AbGroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O], _structure16: AbGroup[P], _structure17: AbGroup[Q], _structure18: AbGroup[R], _structure19: AbGroup[S], _structure20: AbGroup[T]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new AbGroupProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def AbGroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O], _structure16: AbGroup[P], _structure17: AbGroup[Q], _structure18: AbGroup[R], _structure19: AbGroup[S], _structure20: AbGroup[T], _structure21: AbGroup[U]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new AbGroupProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def AbGroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: AbGroup[A], _structure2: AbGroup[B], _structure3: AbGroup[C], _structure4: AbGroup[D], _structure5: AbGroup[E], _structure6: AbGroup[F], _structure7: AbGroup[G], _structure8: AbGroup[H], _structure9: AbGroup[I], _structure10: AbGroup[J], _structure11: AbGroup[K], _structure12: AbGroup[L], _structure13: AbGroup[M], _structure14: AbGroup[N], _structure15: AbGroup[O], _structure16: AbGroup[P], _structure17: AbGroup[Q], _structure18: AbGroup[R], _structure19: AbGroup[S], _structure20: AbGroup[T], _structure21: AbGroup[U], _structure22: AbGroup[V]): AbGroup[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new AbGroupProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait SemiringProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Semiring[(A, B)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  def plus(x0: (A, B), x1: (A, B)): (A, B) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2)) }
  def times(x0: (A, B), x1: (A, B)): (A, B) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2)) }
  override def pow(x0: (A, B), x1: Int): (A, B) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1)) }
}
trait SemiringProduct3[A, B, C] extends Semiring[(A, B, C)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  def plus(x0: (A, B, C), x1: (A, B, C)): (A, B, C) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3)) }
  def times(x0: (A, B, C), x1: (A, B, C)): (A, B, C) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3)) }
  override def pow(x0: (A, B, C), x1: Int): (A, B, C) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1)) }
}
trait SemiringProduct4[A, B, C, D] extends Semiring[(A, B, C, D)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  def plus(x0: (A, B, C, D), x1: (A, B, C, D)): (A, B, C, D) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4)) }
  def times(x0: (A, B, C, D), x1: (A, B, C, D)): (A, B, C, D) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4)) }
  override def pow(x0: (A, B, C, D), x1: Int): (A, B, C, D) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1)) }
}
trait SemiringProduct5[A, B, C, D, E] extends Semiring[(A, B, C, D, E)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  def plus(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5)) }
  def times(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5)) }
  override def pow(x0: (A, B, C, D, E), x1: Int): (A, B, C, D, E) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1)) }
}
trait SemiringProduct6[A, B, C, D, E, F] extends Semiring[(A, B, C, D, E, F)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  def plus(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6)) }
  def times(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6)) }
  override def pow(x0: (A, B, C, D, E, F), x1: Int): (A, B, C, D, E, F) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1)) }
}
trait SemiringProduct7[A, B, C, D, E, F, G] extends Semiring[(A, B, C, D, E, F, G)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  def plus(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7)) }
  def times(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7)) }
  override def pow(x0: (A, B, C, D, E, F, G), x1: Int): (A, B, C, D, E, F, G) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1)) }
}
trait SemiringProduct8[A, B, C, D, E, F, G, H] extends Semiring[(A, B, C, D, E, F, G, H)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  def plus(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8)) }
  def times(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8)) }
  override def pow(x0: (A, B, C, D, E, F, G, H), x1: Int): (A, B, C, D, E, F, G, H) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1)) }
}
trait SemiringProduct9[A, B, C, D, E, F, G, H, I] extends Semiring[(A, B, C, D, E, F, G, H, I)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  def plus(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9)) }
  def times(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I), x1: Int): (A, B, C, D, E, F, G, H, I) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1)) }
}
trait SemiringProduct10[A, B, C, D, E, F, G, H, I, J] extends Semiring[(A, B, C, D, E, F, G, H, I, J)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J), x1: Int): (A, B, C, D, E, F, G, H, I, J) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1)) }
}
trait SemiringProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: Int): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1)) }
}
trait SemiringProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1)) }
}
trait SemiringProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1)) }
}
trait SemiringProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1)) }
}
trait SemiringProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1)) }
}
trait SemiringProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  implicit def structure16: Semiring[P]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15), structure16.plus(x0._16, x1._16)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15), structure16.times(x0._16, x1._16)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1), structure16.pow(x0._16, x1)) }
}
trait SemiringProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  implicit def structure16: Semiring[P]
  implicit def structure17: Semiring[Q]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15), structure16.plus(x0._16, x1._16), structure17.plus(x0._17, x1._17)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15), structure16.times(x0._16, x1._16), structure17.times(x0._17, x1._17)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1), structure16.pow(x0._16, x1), structure17.pow(x0._17, x1)) }
}
trait SemiringProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  implicit def structure16: Semiring[P]
  implicit def structure17: Semiring[Q]
  implicit def structure18: Semiring[R]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15), structure16.plus(x0._16, x1._16), structure17.plus(x0._17, x1._17), structure18.plus(x0._18, x1._18)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15), structure16.times(x0._16, x1._16), structure17.times(x0._17, x1._17), structure18.times(x0._18, x1._18)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1), structure16.pow(x0._16, x1), structure17.pow(x0._17, x1), structure18.pow(x0._18, x1)) }
}
trait SemiringProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  implicit def structure16: Semiring[P]
  implicit def structure17: Semiring[Q]
  implicit def structure18: Semiring[R]
  implicit def structure19: Semiring[S]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15), structure16.plus(x0._16, x1._16), structure17.plus(x0._17, x1._17), structure18.plus(x0._18, x1._18), structure19.plus(x0._19, x1._19)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15), structure16.times(x0._16, x1._16), structure17.times(x0._17, x1._17), structure18.times(x0._18, x1._18), structure19.times(x0._19, x1._19)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1), structure16.pow(x0._16, x1), structure17.pow(x0._17, x1), structure18.pow(x0._18, x1), structure19.pow(x0._19, x1)) }
}
trait SemiringProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  implicit def structure16: Semiring[P]
  implicit def structure17: Semiring[Q]
  implicit def structure18: Semiring[R]
  implicit def structure19: Semiring[S]
  implicit def structure20: Semiring[T]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15), structure16.plus(x0._16, x1._16), structure17.plus(x0._17, x1._17), structure18.plus(x0._18, x1._18), structure19.plus(x0._19, x1._19), structure20.plus(x0._20, x1._20)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15), structure16.times(x0._16, x1._16), structure17.times(x0._17, x1._17), structure18.times(x0._18, x1._18), structure19.times(x0._19, x1._19), structure20.times(x0._20, x1._20)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1), structure16.pow(x0._16, x1), structure17.pow(x0._17, x1), structure18.pow(x0._18, x1), structure19.pow(x0._19, x1), structure20.pow(x0._20, x1)) }
}
trait SemiringProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  implicit def structure16: Semiring[P]
  implicit def structure17: Semiring[Q]
  implicit def structure18: Semiring[R]
  implicit def structure19: Semiring[S]
  implicit def structure20: Semiring[T]
  implicit def structure21: Semiring[U]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15), structure16.plus(x0._16, x1._16), structure17.plus(x0._17, x1._17), structure18.plus(x0._18, x1._18), structure19.plus(x0._19, x1._19), structure20.plus(x0._20, x1._20), structure21.plus(x0._21, x1._21)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15), structure16.times(x0._16, x1._16), structure17.times(x0._17, x1._17), structure18.times(x0._18, x1._18), structure19.times(x0._19, x1._19), structure20.times(x0._20, x1._20), structure21.times(x0._21, x1._21)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1), structure16.pow(x0._16, x1), structure17.pow(x0._17, x1), structure18.pow(x0._18, x1), structure19.pow(x0._19, x1), structure20.pow(x0._20, x1), structure21.pow(x0._21, x1)) }
}
trait SemiringProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
  implicit def structure1: Semiring[A]
  implicit def structure2: Semiring[B]
  implicit def structure3: Semiring[C]
  implicit def structure4: Semiring[D]
  implicit def structure5: Semiring[E]
  implicit def structure6: Semiring[F]
  implicit def structure7: Semiring[G]
  implicit def structure8: Semiring[H]
  implicit def structure9: Semiring[I]
  implicit def structure10: Semiring[J]
  implicit def structure11: Semiring[K]
  implicit def structure12: Semiring[L]
  implicit def structure13: Semiring[M]
  implicit def structure14: Semiring[N]
  implicit def structure15: Semiring[O]
  implicit def structure16: Semiring[P]
  implicit def structure17: Semiring[Q]
  implicit def structure18: Semiring[R]
  implicit def structure19: Semiring[S]
  implicit def structure20: Semiring[T]
  implicit def structure21: Semiring[U]
  implicit def structure22: Semiring[V]
  def plus(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.plus(x0._1, x1._1), structure2.plus(x0._2, x1._2), structure3.plus(x0._3, x1._3), structure4.plus(x0._4, x1._4), structure5.plus(x0._5, x1._5), structure6.plus(x0._6, x1._6), structure7.plus(x0._7, x1._7), structure8.plus(x0._8, x1._8), structure9.plus(x0._9, x1._9), structure10.plus(x0._10, x1._10), structure11.plus(x0._11, x1._11), structure12.plus(x0._12, x1._12), structure13.plus(x0._13, x1._13), structure14.plus(x0._14, x1._14), structure15.plus(x0._15, x1._15), structure16.plus(x0._16, x1._16), structure17.plus(x0._17, x1._17), structure18.plus(x0._18, x1._18), structure19.plus(x0._19, x1._19), structure20.plus(x0._20, x1._20), structure21.plus(x0._21, x1._21), structure22.plus(x0._22, x1._22)) }
  def times(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.times(x0._1, x1._1), structure2.times(x0._2, x1._2), structure3.times(x0._3, x1._3), structure4.times(x0._4, x1._4), structure5.times(x0._5, x1._5), structure6.times(x0._6, x1._6), structure7.times(x0._7, x1._7), structure8.times(x0._8, x1._8), structure9.times(x0._9, x1._9), structure10.times(x0._10, x1._10), structure11.times(x0._11, x1._11), structure12.times(x0._12, x1._12), structure13.times(x0._13, x1._13), structure14.times(x0._14, x1._14), structure15.times(x0._15, x1._15), structure16.times(x0._16, x1._16), structure17.times(x0._17, x1._17), structure18.times(x0._18, x1._18), structure19.times(x0._19, x1._19), structure20.times(x0._20, x1._20), structure21.times(x0._21, x1._21), structure22.times(x0._22, x1._22)) }
  override def pow(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.pow(x0._1, x1), structure2.pow(x0._2, x1), structure3.pow(x0._3, x1), structure4.pow(x0._4, x1), structure5.pow(x0._5, x1), structure6.pow(x0._6, x1), structure7.pow(x0._7, x1), structure8.pow(x0._8, x1), structure9.pow(x0._9, x1), structure10.pow(x0._10, x1), structure11.pow(x0._11, x1), structure12.pow(x0._12, x1), structure13.pow(x0._13, x1), structure14.pow(x0._14, x1), structure15.pow(x0._15, x1), structure16.pow(x0._16, x1), structure17.pow(x0._17, x1), structure18.pow(x0._18, x1), structure19.pow(x0._19, x1), structure20.pow(x0._20, x1), structure21.pow(x0._21, x1), structure22.pow(x0._22, x1)) }
}
trait SemiringProductImplicits {
  implicit def SemiringProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Semiring[A], _structure2: Semiring[B]): Semiring[(A, B)] = {
    new SemiringProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def SemiringProduct3[A, B, C](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C]): Semiring[(A, B, C)] = {
    new SemiringProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def SemiringProduct4[A, B, C, D](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D]): Semiring[(A, B, C, D)] = {
    new SemiringProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def SemiringProduct5[A, B, C, D, E](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E]): Semiring[(A, B, C, D, E)] = {
    new SemiringProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def SemiringProduct6[A, B, C, D, E, F](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F]): Semiring[(A, B, C, D, E, F)] = {
    new SemiringProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def SemiringProduct7[A, B, C, D, E, F, G](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G]): Semiring[(A, B, C, D, E, F, G)] = {
    new SemiringProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def SemiringProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H]): Semiring[(A, B, C, D, E, F, G, H)] = {
    new SemiringProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def SemiringProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I]): Semiring[(A, B, C, D, E, F, G, H, I)] = {
    new SemiringProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def SemiringProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J]): Semiring[(A, B, C, D, E, F, G, H, I, J)] = {
    new SemiringProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def SemiringProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K]): Semiring[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new SemiringProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def SemiringProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new SemiringProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def SemiringProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new SemiringProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def SemiringProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new SemiringProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def SemiringProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new SemiringProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def SemiringProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O], _structure16: Semiring[P]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new SemiringProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def SemiringProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O], _structure16: Semiring[P], _structure17: Semiring[Q]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new SemiringProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def SemiringProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O], _structure16: Semiring[P], _structure17: Semiring[Q], _structure18: Semiring[R]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new SemiringProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def SemiringProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O], _structure16: Semiring[P], _structure17: Semiring[Q], _structure18: Semiring[R], _structure19: Semiring[S]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new SemiringProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def SemiringProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O], _structure16: Semiring[P], _structure17: Semiring[Q], _structure18: Semiring[R], _structure19: Semiring[S], _structure20: Semiring[T]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new SemiringProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def SemiringProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O], _structure16: Semiring[P], _structure17: Semiring[Q], _structure18: Semiring[R], _structure19: Semiring[S], _structure20: Semiring[T], _structure21: Semiring[U]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new SemiringProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def SemiringProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Semiring[A], _structure2: Semiring[B], _structure3: Semiring[C], _structure4: Semiring[D], _structure5: Semiring[E], _structure6: Semiring[F], _structure7: Semiring[G], _structure8: Semiring[H], _structure9: Semiring[I], _structure10: Semiring[J], _structure11: Semiring[K], _structure12: Semiring[L], _structure13: Semiring[M], _structure14: Semiring[N], _structure15: Semiring[O], _structure16: Semiring[P], _structure17: Semiring[Q], _structure18: Semiring[R], _structure19: Semiring[S], _structure20: Semiring[T], _structure21: Semiring[U], _structure22: Semiring[V]): Semiring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new SemiringProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait RngProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Rng[(A, B)] with SemiringProduct2[A, B] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  def zero: (A, B) = (structure1.zero, structure2.zero)
  def negate(x0: (A, B)): (A, B) = { (structure1.negate(x0._1), structure2.negate(x0._2)) }
}
trait RngProduct3[A, B, C] extends Rng[(A, B, C)] with SemiringProduct3[A, B, C] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  def zero: (A, B, C) = (structure1.zero, structure2.zero, structure3.zero)
  def negate(x0: (A, B, C)): (A, B, C) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3)) }
}
trait RngProduct4[A, B, C, D] extends Rng[(A, B, C, D)] with SemiringProduct4[A, B, C, D] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  def zero: (A, B, C, D) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero)
  def negate(x0: (A, B, C, D)): (A, B, C, D) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4)) }
}
trait RngProduct5[A, B, C, D, E] extends Rng[(A, B, C, D, E)] with SemiringProduct5[A, B, C, D, E] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  def zero: (A, B, C, D, E) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero)
  def negate(x0: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5)) }
}
trait RngProduct6[A, B, C, D, E, F] extends Rng[(A, B, C, D, E, F)] with SemiringProduct6[A, B, C, D, E, F] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  def zero: (A, B, C, D, E, F) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero)
  def negate(x0: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6)) }
}
trait RngProduct7[A, B, C, D, E, F, G] extends Rng[(A, B, C, D, E, F, G)] with SemiringProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  def zero: (A, B, C, D, E, F, G) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero)
  def negate(x0: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7)) }
}
trait RngProduct8[A, B, C, D, E, F, G, H] extends Rng[(A, B, C, D, E, F, G, H)] with SemiringProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  def zero: (A, B, C, D, E, F, G, H) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero)
  def negate(x0: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8)) }
}
trait RngProduct9[A, B, C, D, E, F, G, H, I] extends Rng[(A, B, C, D, E, F, G, H, I)] with SemiringProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  def zero: (A, B, C, D, E, F, G, H, I) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9)) }
}
trait RngProduct10[A, B, C, D, E, F, G, H, I, J] extends Rng[(A, B, C, D, E, F, G, H, I, J)] with SemiringProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  def zero: (A, B, C, D, E, F, G, H, I, J) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10)) }
}
trait RngProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Rng[(A, B, C, D, E, F, G, H, I, J, K)] with SemiringProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  def zero: (A, B, C, D, E, F, G, H, I, J, K) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11)) }
}
trait RngProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L)] with SemiringProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12)) }
}
trait RngProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with SemiringProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13)) }
}
trait RngProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with SemiringProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14)) }
}
trait RngProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with SemiringProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15)) }
}
trait RngProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with SemiringProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  implicit def structure16: Rng[P]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15), structure16.negate(x0._16)) }
}
trait RngProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with SemiringProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  implicit def structure16: Rng[P]
  implicit def structure17: Rng[Q]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15), structure16.negate(x0._16), structure17.negate(x0._17)) }
}
trait RngProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with SemiringProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  implicit def structure16: Rng[P]
  implicit def structure17: Rng[Q]
  implicit def structure18: Rng[R]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15), structure16.negate(x0._16), structure17.negate(x0._17), structure18.negate(x0._18)) }
}
trait RngProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with SemiringProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  implicit def structure16: Rng[P]
  implicit def structure17: Rng[Q]
  implicit def structure18: Rng[R]
  implicit def structure19: Rng[S]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15), structure16.negate(x0._16), structure17.negate(x0._17), structure18.negate(x0._18), structure19.negate(x0._19)) }
}
trait RngProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with SemiringProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  implicit def structure16: Rng[P]
  implicit def structure17: Rng[Q]
  implicit def structure18: Rng[R]
  implicit def structure19: Rng[S]
  implicit def structure20: Rng[T]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero, structure20.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15), structure16.negate(x0._16), structure17.negate(x0._17), structure18.negate(x0._18), structure19.negate(x0._19), structure20.negate(x0._20)) }
}
trait RngProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with SemiringProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  implicit def structure16: Rng[P]
  implicit def structure17: Rng[Q]
  implicit def structure18: Rng[R]
  implicit def structure19: Rng[S]
  implicit def structure20: Rng[T]
  implicit def structure21: Rng[U]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero, structure20.zero, structure21.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15), structure16.negate(x0._16), structure17.negate(x0._17), structure18.negate(x0._18), structure19.negate(x0._19), structure20.negate(x0._20), structure21.negate(x0._21)) }
}
trait RngProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with SemiringProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: Rng[A]
  implicit def structure2: Rng[B]
  implicit def structure3: Rng[C]
  implicit def structure4: Rng[D]
  implicit def structure5: Rng[E]
  implicit def structure6: Rng[F]
  implicit def structure7: Rng[G]
  implicit def structure8: Rng[H]
  implicit def structure9: Rng[I]
  implicit def structure10: Rng[J]
  implicit def structure11: Rng[K]
  implicit def structure12: Rng[L]
  implicit def structure13: Rng[M]
  implicit def structure14: Rng[N]
  implicit def structure15: Rng[O]
  implicit def structure16: Rng[P]
  implicit def structure17: Rng[Q]
  implicit def structure18: Rng[R]
  implicit def structure19: Rng[S]
  implicit def structure20: Rng[T]
  implicit def structure21: Rng[U]
  implicit def structure22: Rng[V]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero, structure20.zero, structure21.zero, structure22.zero)
  def negate(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.negate(x0._1), structure2.negate(x0._2), structure3.negate(x0._3), structure4.negate(x0._4), structure5.negate(x0._5), structure6.negate(x0._6), structure7.negate(x0._7), structure8.negate(x0._8), structure9.negate(x0._9), structure10.negate(x0._10), structure11.negate(x0._11), structure12.negate(x0._12), structure13.negate(x0._13), structure14.negate(x0._14), structure15.negate(x0._15), structure16.negate(x0._16), structure17.negate(x0._17), structure18.negate(x0._18), structure19.negate(x0._19), structure20.negate(x0._20), structure21.negate(x0._21), structure22.negate(x0._22)) }
}
trait RngProductImplicits {
  implicit def RngProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Rng[A], _structure2: Rng[B]): Rng[(A, B)] = {
    new RngProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def RngProduct3[A, B, C](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C]): Rng[(A, B, C)] = {
    new RngProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def RngProduct4[A, B, C, D](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D]): Rng[(A, B, C, D)] = {
    new RngProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def RngProduct5[A, B, C, D, E](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E]): Rng[(A, B, C, D, E)] = {
    new RngProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def RngProduct6[A, B, C, D, E, F](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F]): Rng[(A, B, C, D, E, F)] = {
    new RngProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def RngProduct7[A, B, C, D, E, F, G](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G]): Rng[(A, B, C, D, E, F, G)] = {
    new RngProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def RngProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H]): Rng[(A, B, C, D, E, F, G, H)] = {
    new RngProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def RngProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I]): Rng[(A, B, C, D, E, F, G, H, I)] = {
    new RngProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def RngProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J]): Rng[(A, B, C, D, E, F, G, H, I, J)] = {
    new RngProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def RngProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K]): Rng[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new RngProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def RngProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new RngProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def RngProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new RngProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def RngProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new RngProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def RngProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new RngProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def RngProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O], _structure16: Rng[P]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new RngProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def RngProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O], _structure16: Rng[P], _structure17: Rng[Q]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new RngProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def RngProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O], _structure16: Rng[P], _structure17: Rng[Q], _structure18: Rng[R]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new RngProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def RngProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O], _structure16: Rng[P], _structure17: Rng[Q], _structure18: Rng[R], _structure19: Rng[S]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new RngProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def RngProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O], _structure16: Rng[P], _structure17: Rng[Q], _structure18: Rng[R], _structure19: Rng[S], _structure20: Rng[T]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new RngProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def RngProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O], _structure16: Rng[P], _structure17: Rng[Q], _structure18: Rng[R], _structure19: Rng[S], _structure20: Rng[T], _structure21: Rng[U]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new RngProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def RngProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Rng[A], _structure2: Rng[B], _structure3: Rng[C], _structure4: Rng[D], _structure5: Rng[E], _structure6: Rng[F], _structure7: Rng[G], _structure8: Rng[H], _structure9: Rng[I], _structure10: Rng[J], _structure11: Rng[K], _structure12: Rng[L], _structure13: Rng[M], _structure14: Rng[N], _structure15: Rng[O], _structure16: Rng[P], _structure17: Rng[Q], _structure18: Rng[R], _structure19: Rng[S], _structure20: Rng[T], _structure21: Rng[U], _structure22: Rng[V]): Rng[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new RngProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait RigProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Rig[(A, B)] with SemiringProduct2[A, B] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  def zero: (A, B) = (structure1.zero, structure2.zero)
  def one: (A, B) = (structure1.one, structure2.one)
}
trait RigProduct3[A, B, C] extends Rig[(A, B, C)] with SemiringProduct3[A, B, C] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  def zero: (A, B, C) = (structure1.zero, structure2.zero, structure3.zero)
  def one: (A, B, C) = (structure1.one, structure2.one, structure3.one)
}
trait RigProduct4[A, B, C, D] extends Rig[(A, B, C, D)] with SemiringProduct4[A, B, C, D] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  def zero: (A, B, C, D) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero)
  def one: (A, B, C, D) = (structure1.one, structure2.one, structure3.one, structure4.one)
}
trait RigProduct5[A, B, C, D, E] extends Rig[(A, B, C, D, E)] with SemiringProduct5[A, B, C, D, E] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  def zero: (A, B, C, D, E) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero)
  def one: (A, B, C, D, E) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one)
}
trait RigProduct6[A, B, C, D, E, F] extends Rig[(A, B, C, D, E, F)] with SemiringProduct6[A, B, C, D, E, F] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  def zero: (A, B, C, D, E, F) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero)
  def one: (A, B, C, D, E, F) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one)
}
trait RigProduct7[A, B, C, D, E, F, G] extends Rig[(A, B, C, D, E, F, G)] with SemiringProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  def zero: (A, B, C, D, E, F, G) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero)
  def one: (A, B, C, D, E, F, G) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one)
}
trait RigProduct8[A, B, C, D, E, F, G, H] extends Rig[(A, B, C, D, E, F, G, H)] with SemiringProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  def zero: (A, B, C, D, E, F, G, H) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero)
  def one: (A, B, C, D, E, F, G, H) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one)
}
trait RigProduct9[A, B, C, D, E, F, G, H, I] extends Rig[(A, B, C, D, E, F, G, H, I)] with SemiringProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  def zero: (A, B, C, D, E, F, G, H, I) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero)
  def one: (A, B, C, D, E, F, G, H, I) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one)
}
trait RigProduct10[A, B, C, D, E, F, G, H, I, J] extends Rig[(A, B, C, D, E, F, G, H, I, J)] with SemiringProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  def zero: (A, B, C, D, E, F, G, H, I, J) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero)
  def one: (A, B, C, D, E, F, G, H, I, J) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one)
}
trait RigProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Rig[(A, B, C, D, E, F, G, H, I, J, K)] with SemiringProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  def zero: (A, B, C, D, E, F, G, H, I, J, K) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one)
}
trait RigProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L)] with SemiringProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one)
}
trait RigProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with SemiringProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one)
}
trait RigProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with SemiringProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one)
}
trait RigProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with SemiringProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one)
}
trait RigProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with SemiringProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  implicit def structure16: Rig[P]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one)
}
trait RigProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with SemiringProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  implicit def structure16: Rig[P]
  implicit def structure17: Rig[Q]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one)
}
trait RigProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with SemiringProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  implicit def structure16: Rig[P]
  implicit def structure17: Rig[Q]
  implicit def structure18: Rig[R]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one)
}
trait RigProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with SemiringProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  implicit def structure16: Rig[P]
  implicit def structure17: Rig[Q]
  implicit def structure18: Rig[R]
  implicit def structure19: Rig[S]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one)
}
trait RigProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with SemiringProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  implicit def structure16: Rig[P]
  implicit def structure17: Rig[Q]
  implicit def structure18: Rig[R]
  implicit def structure19: Rig[S]
  implicit def structure20: Rig[T]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero, structure20.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one, structure20.one)
}
trait RigProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with SemiringProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  implicit def structure16: Rig[P]
  implicit def structure17: Rig[Q]
  implicit def structure18: Rig[R]
  implicit def structure19: Rig[S]
  implicit def structure20: Rig[T]
  implicit def structure21: Rig[U]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero, structure20.zero, structure21.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one, structure20.one, structure21.one)
}
trait RigProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with SemiringProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: Rig[A]
  implicit def structure2: Rig[B]
  implicit def structure3: Rig[C]
  implicit def structure4: Rig[D]
  implicit def structure5: Rig[E]
  implicit def structure6: Rig[F]
  implicit def structure7: Rig[G]
  implicit def structure8: Rig[H]
  implicit def structure9: Rig[I]
  implicit def structure10: Rig[J]
  implicit def structure11: Rig[K]
  implicit def structure12: Rig[L]
  implicit def structure13: Rig[M]
  implicit def structure14: Rig[N]
  implicit def structure15: Rig[O]
  implicit def structure16: Rig[P]
  implicit def structure17: Rig[Q]
  implicit def structure18: Rig[R]
  implicit def structure19: Rig[S]
  implicit def structure20: Rig[T]
  implicit def structure21: Rig[U]
  implicit def structure22: Rig[V]
  def zero: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = (structure1.zero, structure2.zero, structure3.zero, structure4.zero, structure5.zero, structure6.zero, structure7.zero, structure8.zero, structure9.zero, structure10.zero, structure11.zero, structure12.zero, structure13.zero, structure14.zero, structure15.zero, structure16.zero, structure17.zero, structure18.zero, structure19.zero, structure20.zero, structure21.zero, structure22.zero)
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one, structure20.one, structure21.one, structure22.one)
}
trait RigProductImplicits {
  implicit def RigProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Rig[A], _structure2: Rig[B]): Rig[(A, B)] = {
    new RigProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def RigProduct3[A, B, C](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C]): Rig[(A, B, C)] = {
    new RigProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def RigProduct4[A, B, C, D](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D]): Rig[(A, B, C, D)] = {
    new RigProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def RigProduct5[A, B, C, D, E](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E]): Rig[(A, B, C, D, E)] = {
    new RigProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def RigProduct6[A, B, C, D, E, F](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F]): Rig[(A, B, C, D, E, F)] = {
    new RigProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def RigProduct7[A, B, C, D, E, F, G](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G]): Rig[(A, B, C, D, E, F, G)] = {
    new RigProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def RigProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H]): Rig[(A, B, C, D, E, F, G, H)] = {
    new RigProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def RigProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I]): Rig[(A, B, C, D, E, F, G, H, I)] = {
    new RigProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def RigProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J]): Rig[(A, B, C, D, E, F, G, H, I, J)] = {
    new RigProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def RigProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K]): Rig[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new RigProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def RigProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new RigProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def RigProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new RigProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def RigProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new RigProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def RigProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new RigProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def RigProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O], _structure16: Rig[P]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new RigProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def RigProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O], _structure16: Rig[P], _structure17: Rig[Q]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new RigProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def RigProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O], _structure16: Rig[P], _structure17: Rig[Q], _structure18: Rig[R]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new RigProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def RigProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O], _structure16: Rig[P], _structure17: Rig[Q], _structure18: Rig[R], _structure19: Rig[S]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new RigProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def RigProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O], _structure16: Rig[P], _structure17: Rig[Q], _structure18: Rig[R], _structure19: Rig[S], _structure20: Rig[T]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new RigProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def RigProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O], _structure16: Rig[P], _structure17: Rig[Q], _structure18: Rig[R], _structure19: Rig[S], _structure20: Rig[T], _structure21: Rig[U]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new RigProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def RigProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Rig[A], _structure2: Rig[B], _structure3: Rig[C], _structure4: Rig[D], _structure5: Rig[E], _structure6: Rig[F], _structure7: Rig[G], _structure8: Rig[H], _structure9: Rig[I], _structure10: Rig[J], _structure11: Rig[K], _structure12: Rig[L], _structure13: Rig[M], _structure14: Rig[N], _structure15: Rig[O], _structure16: Rig[P], _structure17: Rig[Q], _structure18: Rig[R], _structure19: Rig[S], _structure20: Rig[T], _structure21: Rig[U], _structure22: Rig[V]): Rig[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new RigProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait RingProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Ring[(A, B)] with RngProduct2[A, B] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  override def fromInt(x0: Int): (A, B) = { (structure1.fromInt(x0), structure2.fromInt(x0)) }
  def one: (A, B) = (structure1.one, structure2.one)
}
trait RingProduct3[A, B, C] extends Ring[(A, B, C)] with RngProduct3[A, B, C] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  override def fromInt(x0: Int): (A, B, C) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0)) }
  def one: (A, B, C) = (structure1.one, structure2.one, structure3.one)
}
trait RingProduct4[A, B, C, D] extends Ring[(A, B, C, D)] with RngProduct4[A, B, C, D] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  override def fromInt(x0: Int): (A, B, C, D) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0)) }
  def one: (A, B, C, D) = (structure1.one, structure2.one, structure3.one, structure4.one)
}
trait RingProduct5[A, B, C, D, E] extends Ring[(A, B, C, D, E)] with RngProduct5[A, B, C, D, E] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  override def fromInt(x0: Int): (A, B, C, D, E) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0)) }
  def one: (A, B, C, D, E) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one)
}
trait RingProduct6[A, B, C, D, E, F] extends Ring[(A, B, C, D, E, F)] with RngProduct6[A, B, C, D, E, F] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  override def fromInt(x0: Int): (A, B, C, D, E, F) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0)) }
  def one: (A, B, C, D, E, F) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one)
}
trait RingProduct7[A, B, C, D, E, F, G] extends Ring[(A, B, C, D, E, F, G)] with RngProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one)
}
trait RingProduct8[A, B, C, D, E, F, G, H] extends Ring[(A, B, C, D, E, F, G, H)] with RngProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one)
}
trait RingProduct9[A, B, C, D, E, F, G, H, I] extends Ring[(A, B, C, D, E, F, G, H, I)] with RngProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one)
}
trait RingProduct10[A, B, C, D, E, F, G, H, I, J] extends Ring[(A, B, C, D, E, F, G, H, I, J)] with RngProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one)
}
trait RingProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Ring[(A, B, C, D, E, F, G, H, I, J, K)] with RngProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one)
}
trait RingProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L)] with RngProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one)
}
trait RingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with RngProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one)
}
trait RingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with RngProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one)
}
trait RingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with RngProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one)
}
trait RingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with RngProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  implicit def structure16: Ring[P]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0), structure16.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one)
}
trait RingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with RngProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  implicit def structure16: Ring[P]
  implicit def structure17: Ring[Q]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0), structure16.fromInt(x0), structure17.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one)
}
trait RingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with RngProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  implicit def structure16: Ring[P]
  implicit def structure17: Ring[Q]
  implicit def structure18: Ring[R]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0), structure16.fromInt(x0), structure17.fromInt(x0), structure18.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one)
}
trait RingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with RngProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  implicit def structure16: Ring[P]
  implicit def structure17: Ring[Q]
  implicit def structure18: Ring[R]
  implicit def structure19: Ring[S]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0), structure16.fromInt(x0), structure17.fromInt(x0), structure18.fromInt(x0), structure19.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one)
}
trait RingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with RngProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  implicit def structure16: Ring[P]
  implicit def structure17: Ring[Q]
  implicit def structure18: Ring[R]
  implicit def structure19: Ring[S]
  implicit def structure20: Ring[T]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0), structure16.fromInt(x0), structure17.fromInt(x0), structure18.fromInt(x0), structure19.fromInt(x0), structure20.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one, structure20.one)
}
trait RingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with RngProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  implicit def structure16: Ring[P]
  implicit def structure17: Ring[Q]
  implicit def structure18: Ring[R]
  implicit def structure19: Ring[S]
  implicit def structure20: Ring[T]
  implicit def structure21: Ring[U]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0), structure16.fromInt(x0), structure17.fromInt(x0), structure18.fromInt(x0), structure19.fromInt(x0), structure20.fromInt(x0), structure21.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one, structure20.one, structure21.one)
}
trait RingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with RngProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: Ring[A]
  implicit def structure2: Ring[B]
  implicit def structure3: Ring[C]
  implicit def structure4: Ring[D]
  implicit def structure5: Ring[E]
  implicit def structure6: Ring[F]
  implicit def structure7: Ring[G]
  implicit def structure8: Ring[H]
  implicit def structure9: Ring[I]
  implicit def structure10: Ring[J]
  implicit def structure11: Ring[K]
  implicit def structure12: Ring[L]
  implicit def structure13: Ring[M]
  implicit def structure14: Ring[N]
  implicit def structure15: Ring[O]
  implicit def structure16: Ring[P]
  implicit def structure17: Ring[Q]
  implicit def structure18: Ring[R]
  implicit def structure19: Ring[S]
  implicit def structure20: Ring[T]
  implicit def structure21: Ring[U]
  implicit def structure22: Ring[V]
  override def fromInt(x0: Int): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.fromInt(x0), structure2.fromInt(x0), structure3.fromInt(x0), structure4.fromInt(x0), structure5.fromInt(x0), structure6.fromInt(x0), structure7.fromInt(x0), structure8.fromInt(x0), structure9.fromInt(x0), structure10.fromInt(x0), structure11.fromInt(x0), structure12.fromInt(x0), structure13.fromInt(x0), structure14.fromInt(x0), structure15.fromInt(x0), structure16.fromInt(x0), structure17.fromInt(x0), structure18.fromInt(x0), structure19.fromInt(x0), structure20.fromInt(x0), structure21.fromInt(x0), structure22.fromInt(x0)) }
  def one: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = (structure1.one, structure2.one, structure3.one, structure4.one, structure5.one, structure6.one, structure7.one, structure8.one, structure9.one, structure10.one, structure11.one, structure12.one, structure13.one, structure14.one, structure15.one, structure16.one, structure17.one, structure18.one, structure19.one, structure20.one, structure21.one, structure22.one)
}
trait RingProductImplicits {
  implicit def RingProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Ring[A], _structure2: Ring[B]): Ring[(A, B)] = {
    new RingProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def RingProduct3[A, B, C](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C]): Ring[(A, B, C)] = {
    new RingProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def RingProduct4[A, B, C, D](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D]): Ring[(A, B, C, D)] = {
    new RingProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def RingProduct5[A, B, C, D, E](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E]): Ring[(A, B, C, D, E)] = {
    new RingProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def RingProduct6[A, B, C, D, E, F](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F]): Ring[(A, B, C, D, E, F)] = {
    new RingProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def RingProduct7[A, B, C, D, E, F, G](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G]): Ring[(A, B, C, D, E, F, G)] = {
    new RingProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def RingProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H]): Ring[(A, B, C, D, E, F, G, H)] = {
    new RingProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def RingProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I]): Ring[(A, B, C, D, E, F, G, H, I)] = {
    new RingProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def RingProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J]): Ring[(A, B, C, D, E, F, G, H, I, J)] = {
    new RingProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def RingProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K]): Ring[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new RingProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def RingProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new RingProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def RingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new RingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def RingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new RingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def RingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new RingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def RingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O], _structure16: Ring[P]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new RingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def RingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O], _structure16: Ring[P], _structure17: Ring[Q]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new RingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def RingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O], _structure16: Ring[P], _structure17: Ring[Q], _structure18: Ring[R]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new RingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def RingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O], _structure16: Ring[P], _structure17: Ring[Q], _structure18: Ring[R], _structure19: Ring[S]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new RingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def RingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O], _structure16: Ring[P], _structure17: Ring[Q], _structure18: Ring[R], _structure19: Ring[S], _structure20: Ring[T]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new RingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def RingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O], _structure16: Ring[P], _structure17: Ring[Q], _structure18: Ring[R], _structure19: Ring[S], _structure20: Ring[T], _structure21: Ring[U]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new RingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def RingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Ring[A], _structure2: Ring[B], _structure3: Ring[C], _structure4: Ring[D], _structure5: Ring[E], _structure6: Ring[F], _structure7: Ring[G], _structure8: Ring[H], _structure9: Ring[I], _structure10: Ring[J], _structure11: Ring[K], _structure12: Ring[L], _structure13: Ring[M], _structure14: Ring[N], _structure15: Ring[O], _structure16: Ring[P], _structure17: Ring[Q], _structure18: Ring[R], _structure19: Ring[S], _structure20: Ring[T], _structure21: Ring[U], _structure22: Ring[V]): Ring[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new RingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait EuclideanRingProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends EuclideanRing[(A, B)] with RingProduct2[A, B] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  def quot(x0: (A, B), x1: (A, B)): (A, B) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2)) }
  def mod(x0: (A, B), x1: (A, B)): (A, B) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2)) }
  def gcd(x0: (A, B), x1: (A, B)): (A, B) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2)) }
}
trait EuclideanRingProduct3[A, B, C] extends EuclideanRing[(A, B, C)] with RingProduct3[A, B, C] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  def quot(x0: (A, B, C), x1: (A, B, C)): (A, B, C) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3)) }
  def mod(x0: (A, B, C), x1: (A, B, C)): (A, B, C) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3)) }
  def gcd(x0: (A, B, C), x1: (A, B, C)): (A, B, C) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3)) }
}
trait EuclideanRingProduct4[A, B, C, D] extends EuclideanRing[(A, B, C, D)] with RingProduct4[A, B, C, D] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  def quot(x0: (A, B, C, D), x1: (A, B, C, D)): (A, B, C, D) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4)) }
  def mod(x0: (A, B, C, D), x1: (A, B, C, D)): (A, B, C, D) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4)) }
  def gcd(x0: (A, B, C, D), x1: (A, B, C, D)): (A, B, C, D) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4)) }
}
trait EuclideanRingProduct5[A, B, C, D, E] extends EuclideanRing[(A, B, C, D, E)] with RingProduct5[A, B, C, D, E] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  def quot(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5)) }
  def mod(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5)) }
  def gcd(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5)) }
}
trait EuclideanRingProduct6[A, B, C, D, E, F] extends EuclideanRing[(A, B, C, D, E, F)] with RingProduct6[A, B, C, D, E, F] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  def quot(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6)) }
  def mod(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6)) }
  def gcd(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6)) }
}
trait EuclideanRingProduct7[A, B, C, D, E, F, G] extends EuclideanRing[(A, B, C, D, E, F, G)] with RingProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  def quot(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7)) }
  def mod(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7)) }
  def gcd(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7)) }
}
trait EuclideanRingProduct8[A, B, C, D, E, F, G, H] extends EuclideanRing[(A, B, C, D, E, F, G, H)] with RingProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  def quot(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8)) }
  def mod(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8)) }
  def gcd(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8)) }
}
trait EuclideanRingProduct9[A, B, C, D, E, F, G, H, I] extends EuclideanRing[(A, B, C, D, E, F, G, H, I)] with RingProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  def quot(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9)) }
}
trait EuclideanRingProduct10[A, B, C, D, E, F, G, H, I, J] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J)] with RingProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10)) }
}
trait EuclideanRingProduct11[A, B, C, D, E, F, G, H, I, J, K] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K)] with RingProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11)) }
}
trait EuclideanRingProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L)] with RingProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12)) }
}
trait EuclideanRingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with RingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13)) }
}
trait EuclideanRingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with RingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14)) }
}
trait EuclideanRingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with RingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15)) }
}
trait EuclideanRingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with RingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  implicit def structure16: EuclideanRing[P]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15), structure16.quot(x0._16, x1._16)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15), structure16.mod(x0._16, x1._16)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15), structure16.gcd(x0._16, x1._16)) }
}
trait EuclideanRingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with RingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  implicit def structure16: EuclideanRing[P]
  implicit def structure17: EuclideanRing[Q]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15), structure16.quot(x0._16, x1._16), structure17.quot(x0._17, x1._17)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15), structure16.mod(x0._16, x1._16), structure17.mod(x0._17, x1._17)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15), structure16.gcd(x0._16, x1._16), structure17.gcd(x0._17, x1._17)) }
}
trait EuclideanRingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with RingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  implicit def structure16: EuclideanRing[P]
  implicit def structure17: EuclideanRing[Q]
  implicit def structure18: EuclideanRing[R]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15), structure16.quot(x0._16, x1._16), structure17.quot(x0._17, x1._17), structure18.quot(x0._18, x1._18)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15), structure16.mod(x0._16, x1._16), structure17.mod(x0._17, x1._17), structure18.mod(x0._18, x1._18)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15), structure16.gcd(x0._16, x1._16), structure17.gcd(x0._17, x1._17), structure18.gcd(x0._18, x1._18)) }
}
trait EuclideanRingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with RingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  implicit def structure16: EuclideanRing[P]
  implicit def structure17: EuclideanRing[Q]
  implicit def structure18: EuclideanRing[R]
  implicit def structure19: EuclideanRing[S]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15), structure16.quot(x0._16, x1._16), structure17.quot(x0._17, x1._17), structure18.quot(x0._18, x1._18), structure19.quot(x0._19, x1._19)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15), structure16.mod(x0._16, x1._16), structure17.mod(x0._17, x1._17), structure18.mod(x0._18, x1._18), structure19.mod(x0._19, x1._19)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15), structure16.gcd(x0._16, x1._16), structure17.gcd(x0._17, x1._17), structure18.gcd(x0._18, x1._18), structure19.gcd(x0._19, x1._19)) }
}
trait EuclideanRingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with RingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  implicit def structure16: EuclideanRing[P]
  implicit def structure17: EuclideanRing[Q]
  implicit def structure18: EuclideanRing[R]
  implicit def structure19: EuclideanRing[S]
  implicit def structure20: EuclideanRing[T]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15), structure16.quot(x0._16, x1._16), structure17.quot(x0._17, x1._17), structure18.quot(x0._18, x1._18), structure19.quot(x0._19, x1._19), structure20.quot(x0._20, x1._20)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15), structure16.mod(x0._16, x1._16), structure17.mod(x0._17, x1._17), structure18.mod(x0._18, x1._18), structure19.mod(x0._19, x1._19), structure20.mod(x0._20, x1._20)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15), structure16.gcd(x0._16, x1._16), structure17.gcd(x0._17, x1._17), structure18.gcd(x0._18, x1._18), structure19.gcd(x0._19, x1._19), structure20.gcd(x0._20, x1._20)) }
}
trait EuclideanRingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with RingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  implicit def structure16: EuclideanRing[P]
  implicit def structure17: EuclideanRing[Q]
  implicit def structure18: EuclideanRing[R]
  implicit def structure19: EuclideanRing[S]
  implicit def structure20: EuclideanRing[T]
  implicit def structure21: EuclideanRing[U]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15), structure16.quot(x0._16, x1._16), structure17.quot(x0._17, x1._17), structure18.quot(x0._18, x1._18), structure19.quot(x0._19, x1._19), structure20.quot(x0._20, x1._20), structure21.quot(x0._21, x1._21)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15), structure16.mod(x0._16, x1._16), structure17.mod(x0._17, x1._17), structure18.mod(x0._18, x1._18), structure19.mod(x0._19, x1._19), structure20.mod(x0._20, x1._20), structure21.mod(x0._21, x1._21)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15), structure16.gcd(x0._16, x1._16), structure17.gcd(x0._17, x1._17), structure18.gcd(x0._18, x1._18), structure19.gcd(x0._19, x1._19), structure20.gcd(x0._20, x1._20), structure21.gcd(x0._21, x1._21)) }
}
trait EuclideanRingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with RingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: EuclideanRing[A]
  implicit def structure2: EuclideanRing[B]
  implicit def structure3: EuclideanRing[C]
  implicit def structure4: EuclideanRing[D]
  implicit def structure5: EuclideanRing[E]
  implicit def structure6: EuclideanRing[F]
  implicit def structure7: EuclideanRing[G]
  implicit def structure8: EuclideanRing[H]
  implicit def structure9: EuclideanRing[I]
  implicit def structure10: EuclideanRing[J]
  implicit def structure11: EuclideanRing[K]
  implicit def structure12: EuclideanRing[L]
  implicit def structure13: EuclideanRing[M]
  implicit def structure14: EuclideanRing[N]
  implicit def structure15: EuclideanRing[O]
  implicit def structure16: EuclideanRing[P]
  implicit def structure17: EuclideanRing[Q]
  implicit def structure18: EuclideanRing[R]
  implicit def structure19: EuclideanRing[S]
  implicit def structure20: EuclideanRing[T]
  implicit def structure21: EuclideanRing[U]
  implicit def structure22: EuclideanRing[V]
  def quot(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.quot(x0._1, x1._1), structure2.quot(x0._2, x1._2), structure3.quot(x0._3, x1._3), structure4.quot(x0._4, x1._4), structure5.quot(x0._5, x1._5), structure6.quot(x0._6, x1._6), structure7.quot(x0._7, x1._7), structure8.quot(x0._8, x1._8), structure9.quot(x0._9, x1._9), structure10.quot(x0._10, x1._10), structure11.quot(x0._11, x1._11), structure12.quot(x0._12, x1._12), structure13.quot(x0._13, x1._13), structure14.quot(x0._14, x1._14), structure15.quot(x0._15, x1._15), structure16.quot(x0._16, x1._16), structure17.quot(x0._17, x1._17), structure18.quot(x0._18, x1._18), structure19.quot(x0._19, x1._19), structure20.quot(x0._20, x1._20), structure21.quot(x0._21, x1._21), structure22.quot(x0._22, x1._22)) }
  def mod(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.mod(x0._1, x1._1), structure2.mod(x0._2, x1._2), structure3.mod(x0._3, x1._3), structure4.mod(x0._4, x1._4), structure5.mod(x0._5, x1._5), structure6.mod(x0._6, x1._6), structure7.mod(x0._7, x1._7), structure8.mod(x0._8, x1._8), structure9.mod(x0._9, x1._9), structure10.mod(x0._10, x1._10), structure11.mod(x0._11, x1._11), structure12.mod(x0._12, x1._12), structure13.mod(x0._13, x1._13), structure14.mod(x0._14, x1._14), structure15.mod(x0._15, x1._15), structure16.mod(x0._16, x1._16), structure17.mod(x0._17, x1._17), structure18.mod(x0._18, x1._18), structure19.mod(x0._19, x1._19), structure20.mod(x0._20, x1._20), structure21.mod(x0._21, x1._21), structure22.mod(x0._22, x1._22)) }
  def gcd(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.gcd(x0._1, x1._1), structure2.gcd(x0._2, x1._2), structure3.gcd(x0._3, x1._3), structure4.gcd(x0._4, x1._4), structure5.gcd(x0._5, x1._5), structure6.gcd(x0._6, x1._6), structure7.gcd(x0._7, x1._7), structure8.gcd(x0._8, x1._8), structure9.gcd(x0._9, x1._9), structure10.gcd(x0._10, x1._10), structure11.gcd(x0._11, x1._11), structure12.gcd(x0._12, x1._12), structure13.gcd(x0._13, x1._13), structure14.gcd(x0._14, x1._14), structure15.gcd(x0._15, x1._15), structure16.gcd(x0._16, x1._16), structure17.gcd(x0._17, x1._17), structure18.gcd(x0._18, x1._18), structure19.gcd(x0._19, x1._19), structure20.gcd(x0._20, x1._20), structure21.gcd(x0._21, x1._21), structure22.gcd(x0._22, x1._22)) }
}
trait EuclideanRingProductImplicits {
  implicit def EuclideanRingProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B]): EuclideanRing[(A, B)] = {
    new EuclideanRingProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def EuclideanRingProduct3[A, B, C](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C]): EuclideanRing[(A, B, C)] = {
    new EuclideanRingProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def EuclideanRingProduct4[A, B, C, D](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D]): EuclideanRing[(A, B, C, D)] = {
    new EuclideanRingProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def EuclideanRingProduct5[A, B, C, D, E](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E]): EuclideanRing[(A, B, C, D, E)] = {
    new EuclideanRingProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def EuclideanRingProduct6[A, B, C, D, E, F](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F]): EuclideanRing[(A, B, C, D, E, F)] = {
    new EuclideanRingProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def EuclideanRingProduct7[A, B, C, D, E, F, G](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G]): EuclideanRing[(A, B, C, D, E, F, G)] = {
    new EuclideanRingProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def EuclideanRingProduct8[A, B, C, D, E, F, G, H](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H]): EuclideanRing[(A, B, C, D, E, F, G, H)] = {
    new EuclideanRingProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def EuclideanRingProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I]): EuclideanRing[(A, B, C, D, E, F, G, H, I)] = {
    new EuclideanRingProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def EuclideanRingProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J)] = {
    new EuclideanRingProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def EuclideanRingProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new EuclideanRingProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def EuclideanRingProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new EuclideanRingProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def EuclideanRingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new EuclideanRingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def EuclideanRingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new EuclideanRingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def EuclideanRingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new EuclideanRingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def EuclideanRingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O], _structure16: EuclideanRing[P]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new EuclideanRingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def EuclideanRingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O], _structure16: EuclideanRing[P], _structure17: EuclideanRing[Q]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new EuclideanRingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def EuclideanRingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O], _structure16: EuclideanRing[P], _structure17: EuclideanRing[Q], _structure18: EuclideanRing[R]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new EuclideanRingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def EuclideanRingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O], _structure16: EuclideanRing[P], _structure17: EuclideanRing[Q], _structure18: EuclideanRing[R], _structure19: EuclideanRing[S]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new EuclideanRingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def EuclideanRingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O], _structure16: EuclideanRing[P], _structure17: EuclideanRing[Q], _structure18: EuclideanRing[R], _structure19: EuclideanRing[S], _structure20: EuclideanRing[T]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new EuclideanRingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def EuclideanRingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O], _structure16: EuclideanRing[P], _structure17: EuclideanRing[Q], _structure18: EuclideanRing[R], _structure19: EuclideanRing[S], _structure20: EuclideanRing[T], _structure21: EuclideanRing[U]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new EuclideanRingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def EuclideanRingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: EuclideanRing[A], _structure2: EuclideanRing[B], _structure3: EuclideanRing[C], _structure4: EuclideanRing[D], _structure5: EuclideanRing[E], _structure6: EuclideanRing[F], _structure7: EuclideanRing[G], _structure8: EuclideanRing[H], _structure9: EuclideanRing[I], _structure10: EuclideanRing[J], _structure11: EuclideanRing[K], _structure12: EuclideanRing[L], _structure13: EuclideanRing[M], _structure14: EuclideanRing[N], _structure15: EuclideanRing[O], _structure16: EuclideanRing[P], _structure17: EuclideanRing[Q], _structure18: EuclideanRing[R], _structure19: EuclideanRing[S], _structure20: EuclideanRing[T], _structure21: EuclideanRing[U], _structure22: EuclideanRing[V]): EuclideanRing[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new EuclideanRingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
trait FieldProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B] extends Field[(A, B)] with EuclideanRingProduct2[A, B] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  def div(x0: (A, B), x1: (A, B)): (A, B) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2)) }
  def ceil(x0: (A, B)): (A, B) = { (structure1.ceil(x0._1), structure2.ceil(x0._2)) }
  def floor(x0: (A, B)): (A, B) = { (structure1.floor(x0._1), structure2.floor(x0._2)) }
  def round(x0: (A, B)): (A, B) = { (structure1.round(x0._1), structure2.round(x0._2)) }
  def isWhole(x: (A, B)): Boolean = false
}
trait FieldProduct3[A, B, C] extends Field[(A, B, C)] with EuclideanRingProduct3[A, B, C] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  def div(x0: (A, B, C), x1: (A, B, C)): (A, B, C) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3)) }
  def ceil(x0: (A, B, C)): (A, B, C) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3)) }
  def floor(x0: (A, B, C)): (A, B, C) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3)) }
  def round(x0: (A, B, C)): (A, B, C) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3)) }
  def isWhole(x: (A, B, C)): Boolean = false
}
trait FieldProduct4[A, B, C, D] extends Field[(A, B, C, D)] with EuclideanRingProduct4[A, B, C, D] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  def div(x0: (A, B, C, D), x1: (A, B, C, D)): (A, B, C, D) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4)) }
  def ceil(x0: (A, B, C, D)): (A, B, C, D) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4)) }
  def floor(x0: (A, B, C, D)): (A, B, C, D) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4)) }
  def round(x0: (A, B, C, D)): (A, B, C, D) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4)) }
  def isWhole(x: (A, B, C, D)): Boolean = false
}
trait FieldProduct5[A, B, C, D, E] extends Field[(A, B, C, D, E)] with EuclideanRingProduct5[A, B, C, D, E] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  def div(x0: (A, B, C, D, E), x1: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5)) }
  def ceil(x0: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5)) }
  def floor(x0: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5)) }
  def round(x0: (A, B, C, D, E)): (A, B, C, D, E) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5)) }
  def isWhole(x: (A, B, C, D, E)): Boolean = false
}
trait FieldProduct6[A, B, C, D, E, F] extends Field[(A, B, C, D, E, F)] with EuclideanRingProduct6[A, B, C, D, E, F] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  def div(x0: (A, B, C, D, E, F), x1: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6)) }
  def ceil(x0: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6)) }
  def floor(x0: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6)) }
  def round(x0: (A, B, C, D, E, F)): (A, B, C, D, E, F) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6)) }
  def isWhole(x: (A, B, C, D, E, F)): Boolean = false
}
trait FieldProduct7[A, B, C, D, E, F, G] extends Field[(A, B, C, D, E, F, G)] with EuclideanRingProduct7[A, B, C, D, E, F, G] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  def div(x0: (A, B, C, D, E, F, G), x1: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7)) }
  def ceil(x0: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7)) }
  def floor(x0: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7)) }
  def round(x0: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7)) }
  def isWhole(x: (A, B, C, D, E, F, G)): Boolean = false
}
trait FieldProduct8[A, B, C, D, E, F, G, H] extends Field[(A, B, C, D, E, F, G, H)] with EuclideanRingProduct8[A, B, C, D, E, F, G, H] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  def div(x0: (A, B, C, D, E, F, G, H), x1: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8)) }
  def ceil(x0: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8)) }
  def floor(x0: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8)) }
  def round(x0: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8)) }
  def isWhole(x: (A, B, C, D, E, F, G, H)): Boolean = false
}
trait FieldProduct9[A, B, C, D, E, F, G, H, I] extends Field[(A, B, C, D, E, F, G, H, I)] with EuclideanRingProduct9[A, B, C, D, E, F, G, H, I] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  def div(x0: (A, B, C, D, E, F, G, H, I), x1: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9)) }
  def round(x0: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I)): Boolean = false
}
trait FieldProduct10[A, B, C, D, E, F, G, H, I, J] extends Field[(A, B, C, D, E, F, G, H, I, J)] with EuclideanRingProduct10[A, B, C, D, E, F, G, H, I, J] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  def div(x0: (A, B, C, D, E, F, G, H, I, J), x1: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J)): Boolean = false
}
trait FieldProduct11[A, B, C, D, E, F, G, H, I, J, K] extends Field[(A, B, C, D, E, F, G, H, I, J, K)] with EuclideanRingProduct11[A, B, C, D, E, F, G, H, I, J, K] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K), x1: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K)): Boolean = false
}
trait FieldProduct12[A, B, C, D, E, F, G, H, I, J, K, L] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L)] with EuclideanRingProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L), x1: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L)): Boolean = false
}
trait FieldProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M)] with EuclideanRingProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M)): Boolean = false
}
trait FieldProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] with EuclideanRingProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): Boolean = false
}
trait FieldProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] with EuclideanRingProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): Boolean = false
}
trait FieldProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] with EuclideanRingProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  implicit def structure16: Field[P]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15), structure16.div(x0._16, x1._16)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15), structure16.ceil(x0._16)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15), structure16.floor(x0._16)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15), structure16.round(x0._16)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): Boolean = false
}
trait FieldProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] with EuclideanRingProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  implicit def structure16: Field[P]
  implicit def structure17: Field[Q]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15), structure16.div(x0._16, x1._16), structure17.div(x0._17, x1._17)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15), structure16.ceil(x0._16), structure17.ceil(x0._17)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15), structure16.floor(x0._16), structure17.floor(x0._17)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15), structure16.round(x0._16), structure17.round(x0._17)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): Boolean = false
}
trait FieldProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] with EuclideanRingProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  implicit def structure16: Field[P]
  implicit def structure17: Field[Q]
  implicit def structure18: Field[R]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15), structure16.div(x0._16, x1._16), structure17.div(x0._17, x1._17), structure18.div(x0._18, x1._18)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15), structure16.ceil(x0._16), structure17.ceil(x0._17), structure18.ceil(x0._18)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15), structure16.floor(x0._16), structure17.floor(x0._17), structure18.floor(x0._18)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15), structure16.round(x0._16), structure17.round(x0._17), structure18.round(x0._18)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): Boolean = false
}
trait FieldProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] with EuclideanRingProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  implicit def structure16: Field[P]
  implicit def structure17: Field[Q]
  implicit def structure18: Field[R]
  implicit def structure19: Field[S]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15), structure16.div(x0._16, x1._16), structure17.div(x0._17, x1._17), structure18.div(x0._18, x1._18), structure19.div(x0._19, x1._19)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15), structure16.ceil(x0._16), structure17.ceil(x0._17), structure18.ceil(x0._18), structure19.ceil(x0._19)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15), structure16.floor(x0._16), structure17.floor(x0._17), structure18.floor(x0._18), structure19.floor(x0._19)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15), structure16.round(x0._16), structure17.round(x0._17), structure18.round(x0._18), structure19.round(x0._19)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): Boolean = false
}
trait FieldProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] with EuclideanRingProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  implicit def structure16: Field[P]
  implicit def structure17: Field[Q]
  implicit def structure18: Field[R]
  implicit def structure19: Field[S]
  implicit def structure20: Field[T]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15), structure16.div(x0._16, x1._16), structure17.div(x0._17, x1._17), structure18.div(x0._18, x1._18), structure19.div(x0._19, x1._19), structure20.div(x0._20, x1._20)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15), structure16.ceil(x0._16), structure17.ceil(x0._17), structure18.ceil(x0._18), structure19.ceil(x0._19), structure20.ceil(x0._20)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15), structure16.floor(x0._16), structure17.floor(x0._17), structure18.floor(x0._18), structure19.floor(x0._19), structure20.floor(x0._20)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15), structure16.round(x0._16), structure17.round(x0._17), structure18.round(x0._18), structure19.round(x0._19), structure20.round(x0._20)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): Boolean = false
}
trait FieldProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] with EuclideanRingProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  implicit def structure16: Field[P]
  implicit def structure17: Field[Q]
  implicit def structure18: Field[R]
  implicit def structure19: Field[S]
  implicit def structure20: Field[T]
  implicit def structure21: Field[U]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15), structure16.div(x0._16, x1._16), structure17.div(x0._17, x1._17), structure18.div(x0._18, x1._18), structure19.div(x0._19, x1._19), structure20.div(x0._20, x1._20), structure21.div(x0._21, x1._21)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15), structure16.ceil(x0._16), structure17.ceil(x0._17), structure18.ceil(x0._18), structure19.ceil(x0._19), structure20.ceil(x0._20), structure21.ceil(x0._21)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15), structure16.floor(x0._16), structure17.floor(x0._17), structure18.floor(x0._18), structure19.floor(x0._19), structure20.floor(x0._20), structure21.floor(x0._21)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15), structure16.round(x0._16), structure17.round(x0._17), structure18.round(x0._18), structure19.round(x0._19), structure20.round(x0._20), structure21.round(x0._21)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): Boolean = false
}
trait FieldProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] extends Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] with EuclideanRingProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
  implicit def structure1: Field[A]
  implicit def structure2: Field[B]
  implicit def structure3: Field[C]
  implicit def structure4: Field[D]
  implicit def structure5: Field[E]
  implicit def structure6: Field[F]
  implicit def structure7: Field[G]
  implicit def structure8: Field[H]
  implicit def structure9: Field[I]
  implicit def structure10: Field[J]
  implicit def structure11: Field[K]
  implicit def structure12: Field[L]
  implicit def structure13: Field[M]
  implicit def structure14: Field[N]
  implicit def structure15: Field[O]
  implicit def structure16: Field[P]
  implicit def structure17: Field[Q]
  implicit def structure18: Field[R]
  implicit def structure19: Field[S]
  implicit def structure20: Field[T]
  implicit def structure21: Field[U]
  implicit def structure22: Field[V]
  def div(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), x1: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.div(x0._1, x1._1), structure2.div(x0._2, x1._2), structure3.div(x0._3, x1._3), structure4.div(x0._4, x1._4), structure5.div(x0._5, x1._5), structure6.div(x0._6, x1._6), structure7.div(x0._7, x1._7), structure8.div(x0._8, x1._8), structure9.div(x0._9, x1._9), structure10.div(x0._10, x1._10), structure11.div(x0._11, x1._11), structure12.div(x0._12, x1._12), structure13.div(x0._13, x1._13), structure14.div(x0._14, x1._14), structure15.div(x0._15, x1._15), structure16.div(x0._16, x1._16), structure17.div(x0._17, x1._17), structure18.div(x0._18, x1._18), structure19.div(x0._19, x1._19), structure20.div(x0._20, x1._20), structure21.div(x0._21, x1._21), structure22.div(x0._22, x1._22)) }
  def ceil(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.ceil(x0._1), structure2.ceil(x0._2), structure3.ceil(x0._3), structure4.ceil(x0._4), structure5.ceil(x0._5), structure6.ceil(x0._6), structure7.ceil(x0._7), structure8.ceil(x0._8), structure9.ceil(x0._9), structure10.ceil(x0._10), structure11.ceil(x0._11), structure12.ceil(x0._12), structure13.ceil(x0._13), structure14.ceil(x0._14), structure15.ceil(x0._15), structure16.ceil(x0._16), structure17.ceil(x0._17), structure18.ceil(x0._18), structure19.ceil(x0._19), structure20.ceil(x0._20), structure21.ceil(x0._21), structure22.ceil(x0._22)) }
  def floor(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.floor(x0._1), structure2.floor(x0._2), structure3.floor(x0._3), structure4.floor(x0._4), structure5.floor(x0._5), structure6.floor(x0._6), structure7.floor(x0._7), structure8.floor(x0._8), structure9.floor(x0._9), structure10.floor(x0._10), structure11.floor(x0._11), structure12.floor(x0._12), structure13.floor(x0._13), structure14.floor(x0._14), structure15.floor(x0._15), structure16.floor(x0._16), structure17.floor(x0._17), structure18.floor(x0._18), structure19.floor(x0._19), structure20.floor(x0._20), structure21.floor(x0._21), structure22.floor(x0._22)) }
  def round(x0: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = { (structure1.round(x0._1), structure2.round(x0._2), structure3.round(x0._3), structure4.round(x0._4), structure5.round(x0._5), structure6.round(x0._6), structure7.round(x0._7), structure8.round(x0._8), structure9.round(x0._9), structure10.round(x0._10), structure11.round(x0._11), structure12.round(x0._12), structure13.round(x0._13), structure14.round(x0._14), structure15.round(x0._15), structure16.round(x0._16), structure17.round(x0._17), structure18.round(x0._18), structure19.round(x0._19), structure20.round(x0._20), structure21.round(x0._21), structure22.round(x0._22)) }
  def isWhole(x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): Boolean = false
}
trait FieldProductImplicits {
  implicit def FieldProduct2[@spec(Int,Long,Float,Double) A,@spec(Int,Long,Float,Double) B](implicit _structure1: Field[A], _structure2: Field[B]): Field[(A, B)] = {
    new FieldProduct2[A, B] {
      val structure1 = _structure1
      val structure2 = _structure2
    }
  }
  implicit def FieldProduct3[A, B, C](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C]): Field[(A, B, C)] = {
    new FieldProduct3[A, B, C] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
    }
  }
  implicit def FieldProduct4[A, B, C, D](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D]): Field[(A, B, C, D)] = {
    new FieldProduct4[A, B, C, D] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
    }
  }
  implicit def FieldProduct5[A, B, C, D, E](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E]): Field[(A, B, C, D, E)] = {
    new FieldProduct5[A, B, C, D, E] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
    }
  }
  implicit def FieldProduct6[A, B, C, D, E, F](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F]): Field[(A, B, C, D, E, F)] = {
    new FieldProduct6[A, B, C, D, E, F] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
    }
  }
  implicit def FieldProduct7[A, B, C, D, E, F, G](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G]): Field[(A, B, C, D, E, F, G)] = {
    new FieldProduct7[A, B, C, D, E, F, G] {
      val structure1 = _structure1
      val structure2 = _structure2
      val structure3 = _structure3
      val structure4 = _structure4
      val structure5 = _structure5
      val structure6 = _structure6
      val structure7 = _structure7
    }
  }
  implicit def FieldProduct8[A, B, C, D, E, F, G, H](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H]): Field[(A, B, C, D, E, F, G, H)] = {
    new FieldProduct8[A, B, C, D, E, F, G, H] {
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
  implicit def FieldProduct9[A, B, C, D, E, F, G, H, I](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I]): Field[(A, B, C, D, E, F, G, H, I)] = {
    new FieldProduct9[A, B, C, D, E, F, G, H, I] {
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
  implicit def FieldProduct10[A, B, C, D, E, F, G, H, I, J](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J]): Field[(A, B, C, D, E, F, G, H, I, J)] = {
    new FieldProduct10[A, B, C, D, E, F, G, H, I, J] {
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
  implicit def FieldProduct11[A, B, C, D, E, F, G, H, I, J, K](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K]): Field[(A, B, C, D, E, F, G, H, I, J, K)] = {
    new FieldProduct11[A, B, C, D, E, F, G, H, I, J, K] {
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
  implicit def FieldProduct12[A, B, C, D, E, F, G, H, I, J, K, L](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L]): Field[(A, B, C, D, E, F, G, H, I, J, K, L)] = {
    new FieldProduct12[A, B, C, D, E, F, G, H, I, J, K, L] {
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
  implicit def FieldProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = {
    new FieldProduct13[A, B, C, D, E, F, G, H, I, J, K, L, M] {
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
  implicit def FieldProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = {
    new FieldProduct14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
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
  implicit def FieldProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = {
    new FieldProduct15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
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
  implicit def FieldProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O], _structure16: Field[P]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = {
    new FieldProduct16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
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
  implicit def FieldProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O], _structure16: Field[P], _structure17: Field[Q]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = {
    new FieldProduct17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
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
  implicit def FieldProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O], _structure16: Field[P], _structure17: Field[Q], _structure18: Field[R]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = {
    new FieldProduct18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
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
  implicit def FieldProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O], _structure16: Field[P], _structure17: Field[Q], _structure18: Field[R], _structure19: Field[S]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = {
    new FieldProduct19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
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
  implicit def FieldProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O], _structure16: Field[P], _structure17: Field[Q], _structure18: Field[R], _structure19: Field[S], _structure20: Field[T]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = {
    new FieldProduct20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
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
  implicit def FieldProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O], _structure16: Field[P], _structure17: Field[Q], _structure18: Field[R], _structure19: Field[S], _structure20: Field[T], _structure21: Field[U]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = {
    new FieldProduct21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
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
  implicit def FieldProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit _structure1: Field[A], _structure2: Field[B], _structure3: Field[C], _structure4: Field[D], _structure5: Field[E], _structure6: Field[F], _structure7: Field[G], _structure8: Field[H], _structure9: Field[I], _structure10: Field[J], _structure11: Field[K], _structure12: Field[L], _structure13: Field[M], _structure14: Field[N], _structure15: Field[O], _structure16: Field[P], _structure17: Field[Q], _structure18: Field[R], _structure19: Field[S], _structure20: Field[T], _structure21: Field[U], _structure22: Field[V]): Field[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = {
    new FieldProduct22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
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
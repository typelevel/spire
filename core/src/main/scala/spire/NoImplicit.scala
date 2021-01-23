package spire

import scala.annotation.nowarn

/**
 * `NoImplicit` provides a way to ensure that a particular implicit doesn't
 * exist. It is often useful to work-around annoying ambiguous implicit
 * problems.
 */
final class NoImplicit[A]

object NoImplicit {
  implicit def noImplicit0[A]: NoImplicit[A] = new NoImplicit[A]
  @nowarn
  implicit def noImplicit1[A](implicit ev: A): NoImplicit[A] = new NoImplicit[A]
}


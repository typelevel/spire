package spire

/**
 * `NoImplicit` provides a way to ensure that a particular implicit doesn't
 * exist. It is often useful to work-around annoying ambiguous implicit
 * problems.
 */
final class NoImplicit[A]

object NoImplicit {
  implicit def noImplicit0[A] = new NoImplicit[A]
  implicit def noImplicit1[A](implicit ev: A) = new NoImplicit[A]
}


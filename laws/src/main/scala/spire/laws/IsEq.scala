package spire.laws

sealed trait IsEq[A] {
  def fold[B](whenValid: (A, A) => B, whenInvalid: => B): B
}

object IsEq {

  final class InvalidTestException extends Exception

  protected case class Valid[A](lhs: A, rhs: A) extends IsEq[A] {
    def fold[B](whenValid: (A, A) => B, whenInvalid: => B): B = whenValid(lhs, rhs)
  }
  protected case class Invalid[A]() extends IsEq[A] {
    def fold[B](whenValid: (A, A) => B, whenInvalid: => B): B = whenInvalid
  }

  def valid[A](lhs: A, rhs: A): IsEq[A] = Valid(lhs, rhs)

  def invalid[A]: IsEq[A] = Invalid[A]()
}

package spire
package util

import cats.kernel.Eq

object Opt {
  def apply[A](a: A): Opt[A] = new Opt(a)

  def empty[A]: Opt[A] = new Opt[A](null.asInstanceOf[A])

  // this is a name-based extractor. instead of returning Option[_] it
  // is free to return any type with .isEmpty and .get, i.e. Opt[_].
  //
  // https://hseeberger.wordpress.com/2013/10/04/name-based-extractors-in-scala-2-11/
  def unapply[A](n: Opt[A]): Opt[A] = n

  implicit def Eq[A](implicit ev: Eq[A]): Eq[Opt[A]] = new Eq[Opt[A]] {
    def eqv(x: Opt[A], y: Opt[A]): Boolean =
      if (x.isEmpty) y.isEmpty else ev.eqv(x.ref, y.ref)
  }
}

class Opt[+A](val ref: A) extends AnyVal {

  def isDefined: Boolean = ref != null
  def nonEmpty: Boolean = ref != null
  def isEmpty: Boolean = ref == null

  def get: A = if (ref == null) throw new NoSuchElementException("Opt.empty.get") else ref

  override def toString: String =
    if (ref == null) "Opt.empty" else s"Opt($ref)"

  def filter(f: A => Boolean): Opt[A] =
    if (ref != null && f(ref)) this else Opt.empty

  def map[B](f: A => B): Opt[B] =
    if (ref == null) Opt.empty else Opt(f(ref))

  def flatMap[B](f: A => Opt[B]): Opt[B] =
    if (ref == null) Opt.empty else f(ref)

  def fold[B](b: => B)(f: A => B): B =
    if (ref == null) b else f(ref)


  def getOrElse[B >: A](default: => B): B = if (ref == null) default else ref

  def getOrElseFast[B >: A](default: B): B = if (ref == null) default else ref

  def toOption: Option[A] = if (ref == null) None else Some(ref)

  def toList: List[A] = if (ref == null) Nil else (ref :: Nil)

  def contains[A1 >: A](elem: A1): Boolean = if (ref == null) false else ref == elem

  def exists(p: A => Boolean): Boolean = if (ref == null) false else p(ref)

  def forall(p: A => Boolean): Boolean = if (ref == null) true else p(ref)

  def foreach[U](f: A => U): Unit = if (ref != null) f(ref)

  def iterator: Iterator[A] = if (ref == null) collection.Iterator.empty else collection.Iterator.single(ref)

  def toRight[X](left: => X): Either[X, A] =
    if (ref == null) Left(left) else Right(ref)

  def toLeft[X](right: => X): Either[A, X] =
    if (ref == null) Right(right) else Left(ref)
}

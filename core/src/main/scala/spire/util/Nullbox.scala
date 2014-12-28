package spire.util
 
object Nullbox {
  def apply[A](a: A): Nullbox[A] = new Nullbox(a)
  def empty[A]: Nullbox[A] = new Nullbox[A](null.asInstanceOf[A])

  // name-based extractor, cf. http://hseeberger.github.io/blog/2013/10/04/name-based-extractors-in-scala-2-dot-11/
  def unapply[A](n: Nullbox[A]): Nullbox[A] = n
}

class Nullbox[A](val ref: A) extends AnyVal {
  def isDefined: Boolean = ref != null
  def nonEmpty: Boolean = ref != null
  def isEmpty: Boolean = ref == null

  def get: A = if (ref == null) throw new NoSuchElementException("Nullbox.empty.get") else ref

  override def toString: String =
    if (ref == null) "Nullbox.empty" else s"Nullbox($ref)"
  
  def filter(f: A => Boolean): Nullbox[A] =
    if (ref != null && f(ref)) this else Nullbox.empty
  
  def map[B](f: A => B): Nullbox[B] =
    if (ref == null) Nullbox.empty else Nullbox(f(ref))
  
  def flatMap[B](f: A => Nullbox[B]): Nullbox[B] =
    if (ref == null) Nullbox.empty else f(ref)
  
  def fold[B](b: => B)(f: A => B): B =
    if (ref == null) b else f(ref)


  def getOrElse[B >: A](default: => B): B = if (ref == null) default else ref

  def getOrElseFast[B >: A](default: B): B = if (ref == null) default else ref

  def toOption: Option[A] = if (ref == null) None else Some(ref)
}

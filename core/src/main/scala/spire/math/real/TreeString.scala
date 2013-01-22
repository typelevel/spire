package spire.math.real


sealed trait TreeString {
  def width: Int
  def joinAt: Int
  def tree: List[String]

  override def toString: String = tree mkString "\n"
}

case class ConstTreeString(a: Any) extends TreeString {
  val tree = List(a.toString)
  val width = tree.head.length
  val joinAt = width / 2
}

case class UnaryTreeString(op: String, sub: TreeString) extends TreeString {
  val width = math.max(sub.width, sub.joinAt + op.length + 2)
  def joinAt = sub.joinAt
  val tree = {
    val tail = if (width <= sub.width) sub.tree else {
      val extra = " " * (width - sub.width)
      sub.tree map (_ + extra)
    }
    val label = " " * joinAt + "|" + op + " " * (width - joinAt - 1 - op.length)
    label :: tail
  }
}

case class BinaryTreeString(op: String, lhs: TreeString, rhs: TreeString)
extends TreeString {
  def joinAt = lhs.width
  def width = math.max(lhs.width + 1 + rhs.width, lhs.width + op.length + 2)
  val tree = {
    val extra = " " * (width - lhs.width - 1 - rhs.width)
    val tail = mergedTree map (_ + extra)
    val label = " " * joinAt + "|" + op + " " * (width - joinAt - 1 - op.length)
    val ljoin = " " * lhs.joinAt + "." + "-" * (lhs.width - lhs.joinAt - 1)
    val rjoin = "-" * rhs.joinAt + "." + " " * (width - lhs.width - rhs.joinAt - 2)
    label :: (ljoin + "^" + rjoin) :: tail
  }

  private def mergedTree: List[String] = {
    val apadl = " " * lhs.joinAt
    val apadr = " " * (lhs.width - lhs.joinAt - 1)
    val bpadl = " " * rhs.joinAt
    val bpadr = " " * (rhs.width - rhs.joinAt - 1)

    def m(res: List[String], as: List[String], bs: List[String]): List[String] = (as, bs) match {
      case (Nil, Nil) => res
      case (a :: as, Nil) => m((a + " " + bpadl + "|" + bpadr) :: res, as, Nil)
      case (Nil, b :: bs) => m((apadl + "|" + apadr + " " + b) :: res, Nil, bs)
      case (a :: as, b :: bs) => m((a + " " + b) :: res, as, bs)
    }

    m(Nil, lhs.tree.reverse, rhs.tree.reverse)
  }
}

object TreeString {
  def apply[A: Coexpr](a: A): TreeString = a match {
    case Add(a, b) => BinaryTreeString("+", apply(a), apply(b))
    case Sub(a, b) => BinaryTreeString("-", apply(a), apply(b))
    case Mul(a, b) => BinaryTreeString("*", apply(a), apply(b))
    case Div(a, b) => BinaryTreeString("/", apply(a), apply(b))
    case Neg(a) => UnaryTreeString("-", apply(a))
    case KRoot(a, k) if k == 2 => UnaryTreeString("sqrt", apply(a))
    case KRoot(a, k) => UnaryTreeString(k + "-root", apply(a))
    case IntLit(n) => ConstTreeString(n)
    case BigIntLit(n) => ConstTreeString(n)
  }
}




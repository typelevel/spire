//package numerics.math
//
//import scala.{specialized => spec}
//
//trait PartialOrder[@spec A] extends Eq[A] {
//  def undef(x:A, y:A): Boolean
//
//  def gt(x:A, y:A): Boolean
//  def lt(x:A, y:A): Boolean
//  def gteq(x:A, y:A): Boolean
//  def lteq(x:A, y:A): Boolean
//
//  def compare
//}
//
//object PartialOrder {
//  sealed trait Result
//  case object Before extends Result
//  case object Equal extends Result
//  case object After extends Result
//  case object Undefined extends Result
//}
//
//trait PartialOrderOps[@spec A] {
//  val lhs:A
//  val p:PartialOrder[A]
//
//  def ?==(rhs:A) = p.undef(lhs, rhs)
//
//  def >(rhs:A) = p.gt(lhs, rhs)
//  def >=(rhs:A) = p.gteq(lhs, rhs)
//  def <(rhs:A) = p.lt(lhs, rhs)
//  def <=(rhs:A) = p.lteq(lhs, rhs)
//}
//
//object PartialOrder {
//  implicit object IntPartialOrder extends IntPartialOrder
//  implicit object LongPartialOrder extends LongPartialOrder
//  implicit object FloatPartialOrder extends FloatPartialOrder
//  implicit object DoublePartialOrder extends DoublePartialOrder
//  implicit object BigIntPartialOrder extends BigIntPartialOrder
//  implicit object BigDecimalPartialOrder extends BigDecimalPartialOrder
//  implicit object RationalPartialOrder extends RationalPartialOrder
//}

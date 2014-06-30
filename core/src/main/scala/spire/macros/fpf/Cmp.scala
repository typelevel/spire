package spire.macros.fpf

private[spire] sealed trait Cmp
private[spire] object Cmp {
  case object Lt extends Cmp
  case object Gt extends Cmp
  case object LtEq extends Cmp
  case object GtEq extends Cmp
  case object Eq extends Cmp
}

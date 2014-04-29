package spire.math.fpf

private[fpf] sealed trait Cmp
private[fpf] object Cmp {
  case object Lt extends Cmp
  case object Gt extends Cmp
  case object LtEq extends Cmp
  case object GtEq extends Cmp
  case object Eq extends Cmp
}

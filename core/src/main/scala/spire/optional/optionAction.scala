package spire.optional

import spire.algebra.{Action, LeftAction, RightAction, PartialAction, LeftPartialAction, RightPartialAction}

trait OptionLeftAction[P, G] extends Any with LeftAction[Option[P], Option[G]] {
  implicit def partial: LeftPartialAction[P, G]
  def actl(gOpt: Option[G], pOpt: Option[P]) = gOpt match {
    case Some(g) => pOpt match {
      case Some(p) => partial.partialActl(g, p)
      case None => None
    }
    case None => None
  }
}

trait OptionRightAction[P, G] extends Any with RightAction[Option[P], Option[G]] {
  implicit def partial: RightPartialAction[P, G]
  def actr(pOpt: Option[P], gOpt: Option[G]) = pOpt match {
    case Some(p) => gOpt match {
      case Some(g) => partial.partialActr(p, g)
      case None => None
    }
    case None => None
  }
}

trait OptionActionTrait[P, G] extends Any with Action[Option[P], Option[G]] with OptionLeftAction[P, G] with OptionRightAction[P, G] {
  implicit def partial: PartialAction[P, G]
}

trait optionActionLowPriority {
  implicit def optionLeftAction[P, G](implicit ev: LeftPartialAction[P, G]): LeftAction[Option[P], Option[G]] =
    new OptionLeftAction[P, G] {
      def partial = ev
    }
  implicit def optionRightAction[P, G](implicit ev: RightPartialAction[P, G]): RightAction[Option[P], Option[G]] =
    new OptionRightAction[P, G] {
      def partial = ev
    }
}

object optionAction extends optionActionLowPriority {
  implicit def optionAction[P, G](implicit ev: PartialAction[P, G]): Action[Option[P], Option[G]] =
    new OptionActionTrait[P, G] {
      def partial = ev
    }
}

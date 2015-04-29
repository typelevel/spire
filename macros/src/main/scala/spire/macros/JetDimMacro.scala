package spire.macros

import reflect.macros.Context

object JetDimMacro {
	
	sealed trait PosIntCheckResult
	case class LteqZero(x: Int) extends PosIntCheckResult
	case object NotConstant extends PosIntCheckResult
	
	def apply(c: Context)(dimension: c.Expr[Int]): c.Expr[Int] = {
	
		import c.universe._

		getInt(c)(dimension) match {
			case Right(_)          => reify { dimension.splice }
			case Left(LteqZero(x)) => c.abort(c.enclosingPosition, s"$x must be > 0.")
			case Left(NotConstant) => reify { dimension.splice }
		}
	}

	def getInt(c: Context)(dimension: c.Expr[Int]): Either[PosIntCheckResult, Int] = {
	
		import c.universe._
		
		dimension.tree match {
			case Literal(Constant(x: Int)) => if (x > 0) Right(x) else Left(LteqZero(x))
			case _                         => Left(NotConstant)
		}
	}
}
package spire.macros

import reflect.macros.Context
import spire.math.JetDim

object JetDimMacro {
	
	sealed trait PosIntCheckResult
	case class LteZero(x: Int) extends PosIntCheckResult
	case object NotConstant extends PosIntCheckResult

	def from(dimension: Int): JetDim = JetDim(dimension)

	// side-effecting if the Int is <= 0
	def apply(c: Context)(dimension: c.Expr[Int]): c.Expr[JetDim] = {
	
		import c.universe._

		getInt(c)(dimension) match {
			case Right(x)          => reify { JetDim.from(x) }
			case Left(LteZero(x))  => c.abort(c.enclosingPosition, s"$x must be > 0.")
			case Left(NotConstant) => reify { JetDim.from(dimension.splice) }
		}
	}

	def getInt(c: Context)(dimension: c.Expr[Int]): Either[PosIntCheckResult, Int] = {
	
		import c.universe._
		
		dimension.tree match {
			case Literal(Constant(x)) => {
				val y = x.toString.toInt // https://groups.google.com/forum/#!topic/scalatest-users/guIlCXHbgh8
				if (y > 0) Right(y) else Left(LteZero(y))
			}
			case _                    => Left(NotConstant)
		}
	}

}
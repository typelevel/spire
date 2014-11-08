package spire.matrix.dense

import scala.math.Ordering.Implicits._


/**
 * Many matrix algorithms need to create temporary vectors or matrices within
 * a loop over the columns or the rows of the matrix to operate on. Allocating
 * or deallocating memory at every loop iteration would be very inefficient.
 * This is the  raisons d'être of the parameter WORK passed to many LAPACK
 * routines: client code is required to allocate enough memory for the biggest
 * temporary to fit in WORK and then pass its address to the LAPACK routine.
 * In turn, each LAPACK routine will pass WORK to the subroutine that needs it.
 * We keep the same scheme but we use an implicit argument `work` instead,
 * of type Scratchpad.
 *
 * In the future, client codes using the higher level interfaces should
 * most of the time not concern themselves with these details as those
 * interfaces will be implemented so as to provide a reasonably sized
 * default value for the implicit parameter `work`. However in some cases,
 * it may be more efficient for the client code to create `work` beforehand
 * and pass it to the algorithm. For example, when computing many eigen-
 * decompositions for matrices of varying size, it would be better
 * to create a scratchpad large enough to work with the biggest matrix.
 * Therefore the lower level interfaces (e.g. Hessenberge.DecompositionXXX)
 * do not create a default scratchpad. But they provide an API to query the
 * scratchpad they need, so that client code can create it without having
 * to know about internal algorithmic details.
 **/
class Scratchpad(val spec:ScratchpadSpecs)
{
  spec.validate

  val matrix: Matrix =
    if (spec.matrixDimensions > (0, 0)) Matrix.empty(spec.matrixDimensions._1,
                                                     spec.matrixDimensions._2)
    else null

  val vector:Vector =
    if (spec.vectorLength > 0) Vector.empty(spec.vectorLength) else null
}

object Scratchpad {
  def apply(matrixDimensions:(Int, Int)=(0, 0),
            vectorLength:Int=0) =
    new Scratchpad(ScratchpadSpecs(matrixDimensions, vectorLength))

}

/**
 * Specification for a Scratchpad size
 *
 * This may be used to ease computing the maximum Scratchpad necessary to
 * accomodate several requirements.
 */
case class ScratchpadSpecs(matrixDimensions:(Int, Int) = (0, 0),
                           vectorLength: Int = 0)
{
  def +(other:ScratchpadSpecs) = {
    import scala.math.max
    ScratchpadSpecs(
      (max(matrixDimensions._1, other.matrixDimensions._1),
       max(matrixDimensions._2, other.matrixDimensions._2)),
      max(vectorLength, other.vectorLength))
  }

  def validate = {
    require(matrixDimensions > (0, 0) || vectorLength > 0)
  }
}

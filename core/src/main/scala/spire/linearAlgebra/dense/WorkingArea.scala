package spire.linearAlgebra.dense

/**
 * Many matrix decomposition algorithms need to create temporary vectors
 * or matrices within a loop over the columns or the rows of the matrix
 * to decompose. Allocating and deallocating memory at every loop iteration
 * would be very inefficient. This is the reason for the parameter WORK
 * passed to many LAPACK routines: client code is required to allocate
 * enough memory for the biggest temporary to fit in WORK and then pass its
 * address to the LAPACK routine. In turn, each LAPACK routine will pass WORK
 * to the subroutine that needs it.
 *
 * WorkingArea is an attempt to eliminate the need for such arguments WORK.
 * Before an algorithm main loop, WorkingArea.reserve(n) should be called
 * with n being the length of the largest temporary vector or matrix
 * to be needed. Then internally, spire.linearAlgebra code shall call
 * e.g. WorkingArea.vector(l) to get a block of size l of the working area
 * previously reserved and use it as a vector.
 *
 * In most cases, client codes using the higher level interfaces should not
 * concern themselves with these details. For example, when a client code runs
 * the Hessenberg decomposition, the spire.linearAlgebra code will issue
 * the appropriate call to WorkingArea.reserve. However in some cases,
 * it may be more efficient for the client code to call WorkingArea.reserve
 * beforehand. For example, when computing many Hessenberg decompositions
 * of matrices of varying size, it would be better to call WorkingArea.reserve(n)
 * with n being the size of the biggest temporary object for the biggest matrix.
 * To help with this, the relevant higher level spire.linearAlgebra
 * decompositions feature the largest temporary object size they need.
 */
object WorkingArea {
  var work: Vector = null
  var size = 0

  def reserve(newSize:Int) {
    if(newSize > size) {
      work = new Vector(newSize)
      size = newSize
    }
  }

  def vector(length:Int): VectorBlock = {
    require(length <= work.size)
    work.block(0, length)
  }
}

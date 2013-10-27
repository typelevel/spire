package spire.matrix.dense

/**
 * Formatting used by the toString method of vectors and matrices
 */
object StringFormatting {
  /** Format used to represent matrix elements as strings */
  var elementFormat = "%10.3g"

  /** Whether to lay out matrices as in Mathematica or as ASCII table */
  var useMathematicaFormat = false

  /** Start, separator and end marker for rows */
  def ofRows(useMathematicaFormat:Boolean) =
    if(useMathematicaFormat) ("{", ",", "}") else ("[ ", " ", " ]")

  /** Start, separator and end marker of columns */
  def ofColumns(useMathematicaFormat:Boolean) =
    if(useMathematicaFormat) ("\n{", ",\n", "}\n") else ("\n", "\n", "\n")

  def postprocess(printout:String, useMathematicaFormat:Boolean):String =
    if(useMathematicaFormat) printout.replace("e", "*^") else printout
}

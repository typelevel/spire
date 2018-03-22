package spire.laws.shadows

/** Exception thrown when the computation exceeds a type range.
  * 
  * For example, when shadowed, Byte(100) + Byte(100) will
  * throw this.
  */
final class InvalidTestException extends Exception

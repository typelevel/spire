package numerics.math

trait ConvertableTo[@specialized A] {
  implicit def fromByte(a:Byte): A
  implicit def fromShort(a:Short): A
  implicit def fromInt(a:Int): A
  implicit def fromLong(a:Long): A
  implicit def fromFloat(a:Float): A
  implicit def fromDouble(a:Double): A
  implicit def fromBigInt(a:BigInt): A
  implicit def fromBigDecimal(a:BigDecimal): A
}

trait ConvertableToByte extends ConvertableTo[Byte] {
  implicit def fromByte(a:Byte): Byte = a
  implicit def fromShort(a:Short): Byte = a.toByte
  implicit def fromInt(a:Int): Byte = a.toByte
  implicit def fromLong(a:Long): Byte = a.toByte
  implicit def fromFloat(a:Float): Byte = a.toByte
  implicit def fromDouble(a:Double): Byte = a.toByte
  implicit def fromBigInt(a:BigInt): Byte = a.toByte
  implicit def fromBigDecimal(a:BigDecimal): Byte = a.toByte
}

trait ConvertableToShort extends ConvertableTo[Short] {
  implicit def fromByte(a:Byte): Short = a.toShort
  implicit def fromShort(a:Short): Short = a
  implicit def fromInt(a:Int): Short = a.toShort
  implicit def fromLong(a:Long): Short = a.toShort
  implicit def fromFloat(a:Float): Short = a.toShort
  implicit def fromDouble(a:Double): Short = a.toShort
  implicit def fromBigInt(a:BigInt): Short = a.toShort
  implicit def fromBigDecimal(a:BigDecimal): Short = a.toShort
}

trait ConvertableToInt extends ConvertableTo[Int] {
  implicit def fromByte(a:Byte): Int = a.toInt
  implicit def fromShort(a:Short): Int = a.toInt
  implicit def fromInt(a:Int): Int = a
  implicit def fromLong(a:Long): Int = a.toInt
  implicit def fromFloat(a:Float): Int = a.toInt
  implicit def fromDouble(a:Double): Int = a.toInt
  implicit def fromBigInt(a:BigInt): Int = a.toInt
  implicit def fromBigDecimal(a:BigDecimal): Int = a.toInt
}

trait ConvertableToLong extends ConvertableTo[Long] {
  implicit def fromByte(a:Byte): Long = a.toLong
  implicit def fromShort(a:Short): Long = a.toLong
  implicit def fromInt(a:Int): Long = a.toLong
  implicit def fromLong(a:Long): Long = a
  implicit def fromFloat(a:Float): Long = a.toLong
  implicit def fromDouble(a:Double): Long = a.toLong
  implicit def fromBigInt(a:BigInt): Long = a.toLong
  implicit def fromBigDecimal(a:BigDecimal): Long = a.toLong
}

trait ConvertableToFloat extends ConvertableTo[Float] {
  implicit def fromByte(a:Byte): Float = a.toFloat
  implicit def fromShort(a:Short): Float = a.toFloat
  implicit def fromInt(a:Int): Float = a.toFloat
  implicit def fromLong(a:Long): Float = a.toFloat
  implicit def fromFloat(a:Float): Float = a
  implicit def fromDouble(a:Double): Float = a.toFloat
  implicit def fromBigInt(a:BigInt): Float = a.toFloat
  implicit def fromBigDecimal(a:BigDecimal): Float = a.toFloat
}

trait ConvertableToDouble extends ConvertableTo[Double] {
  implicit def fromByte(a:Byte): Double = a.toDouble
  implicit def fromShort(a:Short): Double = a.toDouble
  implicit def fromInt(a:Int): Double = a.toDouble
  implicit def fromLong(a:Long): Double = a.toDouble
  implicit def fromFloat(a:Float): Double = a.toDouble
  implicit def fromDouble(a:Double): Double = a
  implicit def fromBigInt(a:BigInt): Double = a.toDouble
  implicit def fromBigDecimal(a:BigDecimal): Double = a.toDouble
}

trait ConvertableToBigInt extends ConvertableTo[BigInt] {
  implicit def fromByte(a:Byte): BigInt = BigInt(a)
  implicit def fromShort(a:Short): BigInt = BigInt(a)
  implicit def fromInt(a:Int): BigInt = BigInt(a)
  implicit def fromLong(a:Long): BigInt = BigInt(a)
  implicit def fromFloat(a:Float): BigInt = BigInt(a.toLong)
  implicit def fromDouble(a:Double): BigInt = BigInt(a.toLong)
  implicit def fromBigInt(a:BigInt): BigInt = a
  implicit def fromBigDecimal(a:BigDecimal): BigInt = a.toBigInt
}

trait ConvertableToBigDecimal extends ConvertableTo[BigDecimal] {
  implicit def fromByte(a:Byte): BigDecimal = BigDecimal(a)
  implicit def fromShort(a:Short): BigDecimal = BigDecimal(a)
  implicit def fromInt(a:Int): BigDecimal = BigDecimal(a)
  implicit def fromLong(a:Long): BigDecimal = BigDecimal(a)
  implicit def fromFloat(a:Float): BigDecimal = BigDecimal(a)
  implicit def fromDouble(a:Double): BigDecimal = BigDecimal(a)
  implicit def fromBigInt(a:BigInt): BigDecimal = BigDecimal(a)
  implicit def fromBigDecimal(a:BigDecimal): BigDecimal = a
}

object ConvertableTo {
  implicit object ConvertableToByte extends ConvertableToByte
  implicit object ConvertableToShort extends ConvertableToShort
  implicit object ConvertableToInt extends ConvertableToInt
  implicit object ConvertableToLong extends ConvertableToLong
  implicit object ConvertableToFloat extends ConvertableToFloat
  implicit object ConvertableToDouble extends ConvertableToDouble
  implicit object ConvertableToBigInt extends ConvertableToBigInt
  implicit object ConvertableToBigDecimal extends ConvertableToBigDecimal
}

trait ConvertableFrom[@specialized A] {
  implicit def toByte(a:A): Byte
  implicit def toShort(a:A): Short
  implicit def toInt(a:A): Int
  implicit def toLong(a:A): Long
  implicit def toFloat(a:A): Float
  implicit def toDouble(a:A): Double
  implicit def toBigInt(a:A): BigInt
  implicit def toBigDecimal(a:A): BigDecimal

  implicit def toString(a:A): String
}

trait ConvertableFromByte extends ConvertableFrom[Byte] {
  implicit def toByte(a:Byte): Byte = a
  implicit def toShort(a:Byte): Short = a.toShort
  implicit def toInt(a:Byte): Int = a.toInt
  implicit def toLong(a:Byte): Long = a.toLong
  implicit def toFloat(a:Byte): Float = a.toFloat
  implicit def toDouble(a:Byte): Double = a.toDouble
  implicit def toBigInt(a:Byte): BigInt = BigInt(a)
  implicit def toBigDecimal(a:Byte): BigDecimal = BigDecimal(a)

  implicit def toString(a:Byte): String = a.toString
}

trait ConvertableFromShort extends ConvertableFrom[Short] {
  implicit def toByte(a:Short): Byte = a.toByte
  implicit def toShort(a:Short): Short = a
  implicit def toInt(a:Short): Int = a.toInt
  implicit def toLong(a:Short): Long = a.toLong
  implicit def toFloat(a:Short): Float = a.toFloat
  implicit def toDouble(a:Short): Double = a.toDouble
  implicit def toBigInt(a:Short): BigInt = BigInt(a)
  implicit def toBigDecimal(a:Short): BigDecimal = BigDecimal(a)

  implicit def toString(a:Short): String = a.toString
}

trait ConvertableFromInt extends ConvertableFrom[Int] {
  implicit def toByte(a:Int): Byte = a.toByte
  implicit def toShort(a:Int): Short = a.toShort
  implicit def toInt(a:Int): Int = a
  implicit def toLong(a:Int): Long = a.toLong
  implicit def toFloat(a:Int): Float = a.toFloat
  implicit def toDouble(a:Int): Double = a.toDouble
  implicit def toBigInt(a:Int): BigInt = BigInt(a)
  implicit def toBigDecimal(a:Int): BigDecimal = BigDecimal(a)

  implicit def toString(a:Int): String = a.toString
}

trait ConvertableFromLong extends ConvertableFrom[Long] {
  implicit def toByte(a:Long): Byte = a.toByte
  implicit def toShort(a:Long): Short = a.toShort
  implicit def toInt(a:Long): Int = a.toInt
  implicit def toLong(a:Long): Long = a
  implicit def toFloat(a:Long): Float = a.toFloat
  implicit def toDouble(a:Long): Double = a.toDouble
  implicit def toBigInt(a:Long): BigInt = BigInt(a)
  implicit def toBigDecimal(a:Long): BigDecimal = BigDecimal(a)

  implicit def toString(a:Long): String = a.toString
}

trait ConvertableFromFloat extends ConvertableFrom[Float] {
  implicit def toByte(a:Float): Byte = a.toByte
  implicit def toShort(a:Float): Short = a.toShort
  implicit def toInt(a:Float): Int = a.toInt
  implicit def toLong(a:Float): Long = a.toLong
  implicit def toFloat(a:Float): Float = a
  implicit def toDouble(a:Float): Double = a.toDouble
  implicit def toBigInt(a:Float): BigInt = BigInt(a.toLong)
  implicit def toBigDecimal(a:Float): BigDecimal = BigDecimal(a)

  implicit def toString(a:Float): String = a.toString
}

trait ConvertableFromDouble extends ConvertableFrom[Double] {
  implicit def toByte(a:Double): Byte = a.toByte
  implicit def toShort(a:Double): Short = a.toShort
  implicit def toInt(a:Double): Int = a.toInt
  implicit def toLong(a:Double): Long = a.toLong
  implicit def toFloat(a:Double): Float = a.toFloat
  implicit def toDouble(a:Double): Double = a
  implicit def toBigInt(a:Double): BigInt = BigInt(a.toLong)
  implicit def toBigDecimal(a:Double): BigDecimal = BigDecimal(a)

  implicit def toString(a:Double): String = a.toString
}

trait ConvertableFromBigInt extends ConvertableFrom[BigInt] {
  implicit def toByte(a:BigInt): Byte = a.toByte
  implicit def toShort(a:BigInt): Short = a.toShort
  implicit def toInt(a:BigInt): Int = a.toInt
  implicit def toLong(a:BigInt): Long = a.toLong
  implicit def toFloat(a:BigInt): Float = a.toFloat
  implicit def toDouble(a:BigInt): Double = a.toDouble
  implicit def toBigInt(a:BigInt): BigInt = a
  implicit def toBigDecimal(a:BigInt): BigDecimal = BigDecimal(a)

  implicit def toString(a:BigInt): String = a.toString
}

trait ConvertableFromBigDecimal extends ConvertableFrom[BigDecimal] {
  implicit def toByte(a:BigDecimal): Byte = a.toByte
  implicit def toShort(a:BigDecimal): Short = a.toShort
  implicit def toInt(a:BigDecimal): Int = a.toInt
  implicit def toLong(a:BigDecimal): Long = a.toLong
  implicit def toFloat(a:BigDecimal): Float = a.toFloat
  implicit def toDouble(a:BigDecimal): Double = a.toDouble
  implicit def toBigInt(a:BigDecimal): BigInt = a.toBigInt
  implicit def toBigDecimal(a:BigDecimal): BigDecimal = a

  implicit def toString(a:BigDecimal): String = a.toString
}

object ConvertableFrom {
  implicit object ConvertableFromByte extends ConvertableFromByte
  implicit object ConvertableFromShort extends ConvertableFromShort
  implicit object ConvertableFromInt extends ConvertableFromInt
  implicit object ConvertableFromLong extends ConvertableFromLong
  implicit object ConvertableFromFloat extends ConvertableFromFloat
  implicit object ConvertableFromDouble extends ConvertableFromDouble
  implicit object ConvertableFromBigInt extends ConvertableFromBigInt
  implicit object ConvertableFromBigDecimal extends ConvertableFromBigDecimal
}

package numerics.math

import scala.{specialized => spec}

trait OrderedRing[@spec(Int,Long,Float,Double) A] extends Ring[A] with Order[A]

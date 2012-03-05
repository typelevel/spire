package numerics.math.real

import numerics.math._


/**
 * A mixin that will move all divisions up to the top of the expression tree,
 * ensuring there is, at most, one division in a `Real`. This is needed for
 * the BFMSS bound.
 */
trait BubbleUpDivs extends RealTransform { self: Real =>
 override def transform(num: Real): Real = super.transform(num) match {
    case Add(Div(a, b), Div(c, d)) => Div(a * d + b * c, b * d)
    case Add(Div(a, b), c) => Div(a + b * c, b)
    case Add(a, Div(b, c)) => Div(a * c + b, c)
    case Sub(Div(a, b), Div(c, d)) => Div(a * d - b * c, b * d)
    case Sub(Div(a, b), c) => Div(a - b * c, b)
    case Sub(a, Div(b, c)) => Div(a * c - b, c)
    case Mul(Div(a, b), Div(c, d)) => Div(a * c, b * d)
    case Mul(Div(a, b), c) => Div(a * c, b)
    case Mul(a, Div(b, c)) => Div(a * b, c)
    case Div(Div(a, b), Div(c, d)) => Div(a * d, b * c)
    case Div(Div(a, b), c) => Div(a, b * c)
    case Div(a, Div(b, c)) => Div(a * c, b)
    case Neg(Div(a, b)) => Div(Neg(a), b)
    case KRoot(Div(a, b), k) => Div(KRoot(a * (b pow (k - 1)), k), b)
    case num => num
  }
}


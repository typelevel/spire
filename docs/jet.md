# Example - Jet

Spire scaladoc is excellent and worth reading - for `Jet` reproduced here to aid discoverability.

While a complete treatment of the mechanics of automatic differentiation is beyond the scope of this header (see http://en.wikipedia.org/wiki/Automatic_differentiation for details), the basic idea is to extend normal arithmetic with an extra element "h" such that <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <msup>
    <mi>h</mi>
    <mn>2</mn>
  </msup>
  <mo>=</mo>
  <mn>0</mn>
</math>

h itself is non zero - an infinitesimal.

Dual numbers are extensions of the real numbers analogous to complex numbers: whereas complex numbers augment the reals by introducing an imaginary unit i such that <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <msup>
    <mi>i</mi>
    <mn>2</mn>
  </msup>
  <mo>=</mo>
  <mo>-</mo>
  <mn>1</mn>
</math>

Dual numbers introduce an "infinitesimal" unit h such that <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <msup>
    <mi>h</mi>
    <mn>2</mn>
  </msup>
  <mo>=</mo>
  <mn>0</mn>
</math>. Analogously to a complex number <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <mi>c</mi>
  <mo>=</mo>
  <mi>x</mi>
  <mo>+</mo>
  <mi>y</mi>
  <mo>&#x22C5;</mo>
  <mi>i</mi>
</math>, a dual number <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <mi>d</mi>
  <mo>=</mo>
  <mi>x</mi>
  <mo>+</mo>
  <mi>y</mi>
  <mo>&#x22C5;</mo>
  <mi>h</mi>
</math> has two components: the "real" component x, and an "infinitesimal" component y. Surprisingly, this leads to a convenient method for computing exact derivatives without needing to manipulate complicated symbolic expressions.

For example, consider the function
<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mrow>
    <mi>f</mi>
    <mo>(</mo>
    <mi>x</mi>
    <mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mo>&#x22C5;</mo>  <!-- Multiplication dot -->
    <mi>x</mi>
  </mrow>
</math>

 evaluated at 10. Using normal arithmetic,

```
f(10 + h) = (10 + h) * (10 + h)
          = 100 + 2 * 10 * h + h * h
          = 100 + 20 * h       +---
                    +-----       |
                    |            +--- This is zero
                    |
                    +----------------- This is df/dx
```
Spire offers us the ability to compute derivatives using Dual numbers through it's `Jet` implementation.

```scala mdoc
import spire._
import spire.math._
import spire.implicits.*
import spire.math.Jet.*

implicit val jd: JetDim = JetDim(1)

val y = Jet(10.0) + Jet.h[Double](0)
y * y

```
Hopefully, you'll recall that the derivative of x^2 is 2x. We would this expect a derivative of 20 at this point.

# Forward Mode Automatic Differentiation


We've already seen that Spire provides the abillity to get the derivate of a function at a point through it's `Jet` class. Through function composition, we can find the partial derivatives of a pretty much arbitrary function at a point.

```scala mdoc:reset

import spire._
import spire.math._
import spire.implicits.*
import _root_.algebra.ring.Field
import spire.algebra.Trig

import spire.math.Jet.*

def softmax[T: Trig: ClassTag](x: Array[T])(implicit f: Field[T]) = {
  val exps = x.map(exp(_))
  val sumExps = exps.foldLeft(f.zero)(_ + _)
  exps.map(t => t  / sumExps)
}

val dim = 4
implicit val jd2: JetDim = JetDim(dim)
val range = (1 to dim).toArray.map(_.toDouble)
val jets = range.zipWithIndex.map{case (i, j) => Jet(i.toDouble) + Jet.h[Double](j)}

softmax[Double](range)
softmax[Jet[Double]](jets)

```

Note that by using Spire's typeclasses, we can write out a function that is valid for both the sorts of numbers we would "normally" want as well as the dual numbers we use here for automatic differentiation.

## For fun

Let's consider an example of using a `Jet` with a complex number. Spire provides support for complex numbers, and we can combine this with `Jet` to perform automatic differentiation on functions involving complex numbers.

```scala mdoc:reset
import spire._
import spire.math._
import spire.implicits._
import spire.math.Jet._
import spire.math.Complex
import spire.algebra.Trig

implicit val jd: JetDim = JetDim(1)

val z = Jet(Complex(2.0, 3.0)) + Jet.h[Complex[Double]](0)
z * z

```
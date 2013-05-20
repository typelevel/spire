package spire
package syntax

import spire.syntax.std._

package object std {
  object int extends IntSyntax
  object long extends LongSyntax
  object double extends DoubleSyntax
  object bigInt extends BigIntSyntax

  object array extends ArraySyntax
  object seq extends SeqSyntax
}

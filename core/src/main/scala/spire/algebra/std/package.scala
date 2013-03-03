package spire.algebra

package object std {
  object any extends AnyInstances

  object boolean extends BooleanInstances
  object char extends CharInstances
  object byte extends ByteInstances
  object short extends ShortInstances
  object int extends IntInstances
  object long extends LongInstances
  object float extends FloatInstances
  object double extends DoubleInstances
  object bigInt extends BigIntInstances
  object bigDecimal extends BigDecimalInstances

  object string extends StringInstances
  object iterable extends IterableInstances
  object array extends ArrayInstances
  object seq extends SeqInstances
  object map extends MapInstances
  object tuples extends ProductInstances
}

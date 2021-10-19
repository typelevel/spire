/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire

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
  object bigInteger extends BigIntegerInstances
  object bigDecimal extends BigDecimalInstances

  object string extends StringInstances
  object iterable extends IterableInstances
  object array extends ArrayInstances
  object seq extends SeqInstances
  object map extends MapInstances
  object tuples extends ProductInstances
  object option extends OptionInstances
  object unit extends UnitInstances
}

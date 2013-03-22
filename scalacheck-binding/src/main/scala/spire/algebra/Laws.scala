package spire.algebra

import scala.collection.SortedMap

import org.scalacheck.{Prop, Properties}

object Laws {

  // implicit scope ftw

  implicit def spireProps2Props(sp: Laws#SpireProperties): Properties = sp.all

}

/**
 * Root trait of the law cake.
 *
 * Defines a wrapper around scalacheck's `Properties` ([[SpireProperties]]),
 * and some default implementations.
 *
 * Extend this trait if you want to define a set of laws.
 */
trait Laws {

  /**
   * This trait abstracts over the various ways how the laws of a type class
   * can depend on the laws of other type classes. An instance of this trait is
   * called a ''property set''.
   *
   * For that matter, we divide type classes into ''kinds'', where the classes
   * of one kind share the number of operations and meaning. For example,
   * `Semigroup`, `Monoid` and `Group` all belong to the same kind. On the
   * other hand, their additive variants also belong to a common kind, but to
   * a different one.
   *
   * Users of this trait should extend the outer trait [[Laws]] and create
   * specialized subtypes for each kind of type class. (See
   * [[DefaultProperties]] for an example.)
   *
   * Consider this example hierarchy:
   * <pre>
   * Semigroup
   *     |   \
   *  Monoid   AdditiveSemigroup
   *     |   \        |
   *  Group     AdditiveMonoid
   *         \        |
   *            AdditiveGroup
   * </pre>
   * They all define their own laws, as well as a couple of parent classes.
   * If we want to check the laws of `AdditiveGroup`, we want to avoid checking
   * properties twice, i.e. do not want to check `Monoid` laws via `Group` and
   * also via `AdditiveMonoid`.
   *
   * To address this problem, we define the parent in the same kind as
   * ''parent'', and other parents as ''bases''. In this example, the parent of
   * `AdditiveGroup` is `Group`, and its only basis is `Group`. On the other
   * hand, the parent of `Group` is `Monoid`, and it does not have any bases.
   *
   * The set of all properties of a certain class is now defined as union of
   * these sets:
   *  - the properties of the class itself
   *  - recursively, the properties of all its parents (ignoring their bases)
   *  - recursively, the set of ''all'' properties of its bases
   *
   * Looking at our example, that means that `AdditiveGroup` includes the
   * `Monoid` law only once, because it is the parent of its basis. The
   * same laws are ignored by its parent `AdditiveMonoid`, hence no redundant
   * checks occur.
   *
   * Of course, classes can have multiple parents and multiple (named) bases.
   * The only requirement here is that ''inside one kind'', the identifier of
   * a property is unique, since duplicates are eliminated. To avoid name
   * clashes ''between different kinds'', the names of properties pulled in
   * via a basis are prefixed with the name of the basis.
   *
   * For better type-safety, ''parents'' are only allowed to come from the
   * same outer instance of [[Laws]], whereas ''bases'' are allowed to come
   * from anywhere.
   */
  trait SpireProperties {
    def name: String
    def bases: Seq[(String, Laws#SpireProperties)]
    def parents: Seq[SpireProperties]
    def props: Seq[(String, Prop)]

    private def collectParentProps: SortedMap[String, Prop] =
      SortedMap(props: _*) ++ parents.flatMap(_.collectParentProps)

    /** Assembles all properties. For the rules, see [[SpireProperties]]. */
    final def all: Properties = new Properties(name) {
      for {
        (baseName, baseProps) ← bases.sortBy(_._1)
        (name, prop) ← baseProps.all.properties
      } property(baseName + ":" + name) = prop

      for ((name, prop) ← collectParentProps)
        property(name) = prop
    }
  }

  /**
   * Convenience trait to mix into subclasses of [[SpireProperties]] for
   * property sets which only have one parent.
   */
  trait HasOneParent { self: SpireProperties =>
    def parent: Option[SpireProperties]

    final def parents = parent.toList
  }

  /**
   * Convenience class for property sets which may have a parent, but no bases.
   */
  class DefaultProperties(
    val name: String,
    val parent: Option[SpireProperties],
    val props: (String, Prop)*
  ) extends SpireProperties with HasOneParent {
    val bases = Seq.empty
  }

  /**
   * Convencience class for property sets without parents and bases.
   */
  class SimpleProperties(
    name: String,
    props: (String, Prop)*
  ) extends DefaultProperties(name, None, props: _*)

  /** Empty property set. */
  def emptyProperties: SpireProperties = new SimpleProperties(
    name = "<empty>"
  )

}

// vim: expandtab:ts=2:sw=2

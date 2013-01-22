object GenProductTypes {
  val spec = "@spec(Int,Long,Float,Double) "

  case class ProductType(structure: String, parentStructure: Option[String], arity: Int) {
    val prefix = "structure"
    def typeName(i: Int): String = (64 + i).toChar.toString
    val types = (1 to arity) map (typeName(_)) mkString ", "
    val specTypes = if (arity == 2) {
      (1 to arity) map { i => spec + typeName(i) } mkString ","
    } else {
      types
    }
    val name = structure + "Product" + arity
  }

  type Block = ProductType => String

  case class Definition(structure: String, parent: Option[String] = None)(val blocks: List[Block]) {
    def ofArity(arity: Int) = ProductType(structure, parent, arity)
  }

  def beginTrait: Block = { tpe =>
    import tpe._

    val parents = ("%s[(%s)]" format (structure, types)) + (parentStructure map { p =>
      " with %sProduct%d[%s]" format (p, arity, types)
    } getOrElse "")

    "trait %s[%s] extends %s {" format (name, specTypes, parents)
  }

  val members: Block = { tpe =>
    import tpe._

    (1 to arity) map { i =>
      "  implicit def %s%d: %s[%s]" format (prefix, i, structure, typeName(i))
    } mkString "\n"
  }

  sealed trait Arg
  case object DelegateArg extends Arg
  case class FixedArg(tpe: String) extends Arg

  def method(methodName: String, args: List[Arg], overrides: Boolean = false): Block = { tpe =>
    import tpe._

    val over = if (overrides) "override " else ""
    args match {
      case Nil =>
        val call = (1 to arity) map { i =>
          "%s%d.%s" format (prefix, i, methodName)
        } mkString ("(", ", ", ")")
        "  %sdef %s: (%s) = %s" format (over, methodName, types, call)

      case args =>
        val arglist = args.zipWithIndex map {
          case (DelegateArg, i) => "x%d: (%s)" format (i, types)
          case (FixedArg(argType), i) => "x%d: %s" format (i, argType)
        } mkString ", "
        val call = (1 to arity) map { j =>
          "%s%d.%s(%s)" format (prefix, j, methodName, args.zipWithIndex map {
            case (DelegateArg, i) => "x%d._%d" format (i, j)
            case (FixedArg(_), i) => "x" + i
          } mkString ", ")
        } mkString ("(", ", ", ")")
        "  %sdef %s(%s): (%s) = { %s }" format (over, methodName, arglist, types, call)
    }
  }

  def const(op: String) = method(op, Nil)
  def unary(op: String) = method(op, DelegateArg :: Nil)
  def binary(op: String) = method(op, DelegateArg :: DelegateArg :: Nil)

  def endTrait: Block = { tpe => "}" }

  def constructor: Block = { tpe =>
    import tpe._

    val implicits = (1 to arity) map { i =>
      "_%s%d: %s[%s]" format (prefix, i, structure, typeName(i))
    } mkString ", "
    val members = (1 to arity) map { i =>
      "      val %s%d = _%s%d" format (prefix, i, prefix, i)
    } mkString "\n"

    """  implicit def %s[%s](implicit %s): %s[(%s)] = {
      |    new %s[%s] {
      |%s
      |    }
      |  }""".stripMargin format (name, specTypes, implicits, structure, types, name, types, members)
  }

  def productTrait(blocks0: List[Block]): Block = { tpe =>
    val blocks = beginTrait :: members :: (blocks0 :+ endTrait)
    blocks map (_(tpe)) mkString "\n"
  }

  def implicitsTrait(start: Int, end: Int): Definition => String = { defn =>
    val implicits = (start to end) map { arity =>
      constructor(defn ofArity arity)
    } mkString "\n"

    """trait %sProductImplicits {
      |%s
      |}""".stripMargin format (defn.structure, implicits)
  }

  def renderStructure(start: Int, end: Int): Definition => String = { defn =>
    val genTrait = productTrait(defn.blocks)
    val traits = (start to end) map { arity =>
      genTrait(defn ofArity arity)
    } mkString "\n"

    "%s\n%s" format (traits, implicitsTrait(start, end)(defn))
  }

  private val disclaimer = """
    |
    |/**************************************************************************
    | * WARNING: This is an auto-generated file. Any changes will most likely  *
    | * be overwritten the next time this file is regenerated.                 *
    | **************************************************************************/
    |
    |""".stripMargin

  def renderAll(pkg: String, start: Int = 2, end: Int = 22): Seq[Definition] => String = { defns =>
    val header = "package %s\nimport scala.{ specialized => spec }" format pkg
    val body = defns map renderStructure(start, end) mkString "\n"

    header + disclaimer + body
  }
}

object ProductTypes {
  import GenProductTypes._

  private val fromInt = method("fromInt", FixedArg("Int") :: Nil, true)
  private val pow = method("pow", DelegateArg :: FixedArg("Int") :: Nil, true)
  private val isWhole: Block = { tpe =>
    "  def isWhole(x: (%s)): Boolean = false" format (tpe.types)
  }

  val semigroup = Definition("Semigroup")(binary("op") :: Nil)
  val monoid = Definition("Monoid", Some("Semigroup"))(const("id") :: Nil)
  val group = Definition("Group", Some("Monoid"))(unary("inverse") :: Nil)
  val abGroup = Definition("AbGroup", Some("Group"))(Nil)
  val semiring = Definition("Semiring")(binary("plus") :: binary("times") :: pow :: Nil)
  val rng = Definition("Rng", Some("Semiring"))(const("zero") :: unary("negate") :: Nil)
  val rig = Definition("Rig", Some("Semiring"))(const("zero") :: const("one") :: Nil)
  val ring = Definition("Ring", Some("Rng"))(fromInt :: const("one") :: Nil)
  val euclideanRing = Definition("EuclideanRing", Some("Ring"))(binary("quot") :: binary("mod") :: binary("gcd") :: Nil)
  val field = Definition("Field", Some("EuclideanRing")) {
    binary("div") :: unary("ceil") :: unary("floor") :: unary("round") :: isWhole :: Nil
  }

  private val eqv: Block = { tpe =>
    import tpe._

    val bool = (1 to arity) map { i =>
      "%s%d.eqv(x0._%d, x1._%d)" format (prefix, i, i, i)
    } mkString " && "
    "  def eqv(x0: (%s), x1: (%s)): Boolean = %s" format (types, types, bool)
  }

  private val compare: Block = { tpe =>
    import tpe._

    def gen(i: Int): String = {
      val indent = "  " * i
      if (i <= arity) {
          """%s  cmp = %s%d.compare(x0._%d, x1._%d)
            |%s  if (cmp != 0) cmp else {
            |%s
            |%s  }""".stripMargin format (indent, prefix, i, i, i, indent, gen(i + 1), indent)
      } else {
        indent + "  0"
      }
    }

    """  def compare(x0: (%s), x1: (%s)): Int = {
      |    var cmp: Int = 0
      |%s
      |}""".stripMargin format (types, types, gen(1))
  }

  val eq = Definition("Eq")(eqv :: Nil)
  val order = Definition("Order", Some("Eq"))(compare :: Nil)

  val algebra = List(semigroup, monoid, group, abGroup, semiring, rng, rig, ring, euclideanRing, field)
  val math = List(eq, order)

  def algebraProductTypes: String = renderAll("spire.algebra", 2, 22)(algebra)
  def mathProductTypes: String = renderAll("spire.math", 2, 22)(math)
}

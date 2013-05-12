package spire.macros

import language.experimental.macros
import scala.reflect.macros.Context

import spire.algebra._

object Auto {
  object scala {
    def semiring[A] = macro ScalaAutoMacros.semiringImpl[A]
    def rig[A](z: A, o: A) = macro ScalaAutoMacros.rigImpl[A]
    def rng[A](z: A) = macro ScalaAutoMacros.rngImpl[A]
    def ring[A](z: A, o: A) = macro ScalaAutoMacros.ringImpl[A]
    def euclideanRing[A](z: A, o: A)(implicit ev: Eq[A]) = macro ScalaAutoMacros.euclideanRingImpl[A]
    def field[A](z: A, o: A)(implicit ev: Eq[A]) = macro ScalaAutoMacros.fieldImpl[A]
    def eq[A] = macro ScalaAutoMacros.eqImpl[A]
    def order[A] = macro ScalaAutoMacros.orderImpl[A]

    object collection {
      def semigroup[A] = macro ScalaAutoMacros.collectionSemigroupImpl[A]
      def monoid[A](z: A) = macro ScalaAutoMacros.collectionMonoidImpl[A]
    }
  }

  object java {
    def semiring[A] = macro JavaAutoMacros.semiringImpl[A]
    def rig[A](z: A, o: A) = macro JavaAutoMacros.rigImpl[A]
    def rng[A](z: A) = macro JavaAutoMacros.rngImpl[A]
    def ring[A](z: A, o: A) = macro JavaAutoMacros.ringImpl[A]
    def euclideanRing[A](z: A, o: A)(implicit ev: Eq[A]) = macro JavaAutoMacros.euclideanRingImpl[A]
    def field[A](z: A, o: A)(implicit ev: Eq[A]) = macro JavaAutoMacros.fieldImpl[A]
    def eq[A] = macro JavaAutoMacros.eqImpl[A]
    def order[A] = macro JavaAutoMacros.orderImpl[A]

    object collection {
      def monoid[A](empty: A) = macro JavaAutoMacros.collectionMonoidImpl[A]
    }
  }
}

abstract class AutoOps {
  val c: Context
  import c.universe._

  def unop[A](name: String, x: String = "x"): c.Expr[A] =
    c.Expr[A](Select(Ident(newTermName(x)), newTermName(name)))

  def binop[A](name: String, x: String = "x", y: String = "y"): c.Expr[A] =
    c.Expr[A](Apply(
      Select(Ident(newTermName(x)), newTermName(name)),
      List(Ident(newTermName(y)))))

  def binopSearch[A: c.WeakTypeTag](names: List[String], x: String = "x", y: String = "y"): Option[c.Expr[A]] =
    names find { name => hasMethod1[A, A, A](name) } map (binop[A](_, x, y))

  def unopSearch[A: c.WeakTypeTag](names: List[String], x: String = "x"): Option[c.Expr[A]] =
    names find { name => hasMethod0[A, A](name) } map (unop[A](_, x))

  def hasMethod0[A: c.WeakTypeTag, B: c.WeakTypeTag](name: String): Boolean = {
    val tpeA = c.weakTypeTag[A].tpe
    val tpeB = c.weakTypeTag[B].tpe
    tpeA.members exists { m =>
      m.isMethod && m.isPublic && m.name.encoded == name && (m.typeSignature match {
        case MethodType(Nil, ret) => ret =:= tpeB
        case _ => false
      })
    }
  }

  def hasMethod1[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag](name: String): Boolean = {
    val tpeA = c.weakTypeTag[A].tpe
    val tpeB = c.weakTypeTag[B].tpe
    val tpeC = c.weakTypeTag[C].tpe
    tpeA.members exists { m =>
      m.isMethod && m.isPublic && m.name.encoded == name && (m.typeSignature match {
        case MethodType(List(param), ret) =>
          param.typeSignature =:= tpeB && ret =:= tpeC
        case _ =>
          false
      })
    }
  }

  def failedSearch(name: String, op: String): c.Expr[Nothing] =
    c.abort(c.enclosingPosition,
      "Couldn't find matching method for op %s (%s)." format (name, op))
}

abstract class AutoAlgebra extends AutoOps { ops =>
  import c.universe._

  def plus[A: c.WeakTypeTag]: c.Expr[A]
  def minus[A: c.WeakTypeTag]: c.Expr[A]
  def times[A: c.WeakTypeTag]: c.Expr[A]
  def negate[A: c.WeakTypeTag]: c.Expr[A]
  def div[A: c.WeakTypeTag]: c.Expr[A]
  def quot[A: c.WeakTypeTag]: c.Expr[A]
  def mod[A: c.WeakTypeTag](stub: => c.Expr[A] = failedSearch("mod", "%")): c.Expr[A]
  def equals: c.Expr[Boolean]
  def compare: c.Expr[Int]

  def Semiring[A: c.WeakTypeTag](): c.Expr[Semiring[A]] = {
    c.universe.reify {
      new Semiring[A] {
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
      }
    }
  }

  def Rig[A: c.WeakTypeTag](z: c.Expr[A], o: c.Expr[A]): c.Expr[Rig[A]] = {
    c.universe.reify {
      new Rig[A] {
        def zero: A = z.splice
        def one: A = o.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
      }
    }
  }

  def Rng[A: c.WeakTypeTag](z: c.Expr[A]): c.Expr[Rng[A]] = {
    c.universe.reify {
      new Rng[A] {
        def zero: A = z.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
        override def minus(x: A, y: A): A = ops.minus[A].splice
        def negate(x: A): A = ops.negate[A].splice
      }
    }
  }

  def Ring[A: c.WeakTypeTag](z: c.Expr[A], o: c.Expr[A]): c.Expr[Ring[A]] = {
    c.universe.reify {
      new Ring[A] {
        def zero: A = z.splice
        def one: A = o.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
        override def minus(x: A, y: A): A = ops.minus[A].splice
        def negate(x: A): A = ops.negate[A].splice
      }
    }
  }

  def EuclideanRing[A: c.WeakTypeTag](z: c.Expr[A], o: c.Expr[A])
      (ev: c.Expr[Eq[A]]): c.Expr[EuclideanRing[A]] = {
    c.universe.reify {
      new EuclideanRing[A] {
        def zero: A = z.splice
        def one: A = o.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
        override def minus(x: A, y: A): A = ops.minus[A].splice
        def negate(x: A): A = ops.negate[A].splice
        def quot(x: A, y: A): A = ops.quot[A].splice
        def mod(x: A, y: A): A = ops.mod[A]().splice
        def gcd(x: A, y: A): A = euclid(x, y)(ev.splice)
      }
    }
  }

  def Field[A: c.WeakTypeTag]
      (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[Field[A]] = {
    c.universe.reify {
      new Field[A] {
        def zero: A = z.splice
        def one: A = o.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
        override def minus(x: A, y: A): A = ops.minus[A].splice
        def negate(x: A): A = ops.negate[A].splice
        def quot(x: A, y: A): A = ops.div[A].splice
        def mod(x: A, y: A): A = ops.mod[A](z).splice
        def gcd(x: A, y: A): A = euclid(x, y)(ev.splice)
        def div(x: A, y: A): A = ops.div[A].splice
      }
    }
  }

  def Eq[A: c.WeakTypeTag](): c.Expr[Eq[A]] = {
    c.universe.reify {
      new Eq[A] {
        def eqv(x: A, y: A): Boolean = ops.equals.splice
      }
    }
  }

  def Order[A: c.WeakTypeTag](): c.Expr[Order[A]] = {
    c.universe.reify {
      new Order[A] {
        override def eqv(x: A, y: A): Boolean = ops.equals.splice
        def compare(x: A, y: A): Int = ops.compare.splice
      }
    }
  }
}

case class ScalaAlgebra[C <: Context](c: C) extends AutoAlgebra {
  def plusplus[A] = binop[A]("$plus$plus")
  def plus[A: c.WeakTypeTag] = binop[A]("$plus")
  def minus[A: c.WeakTypeTag] = binop[A]("$minus")
  def times[A: c.WeakTypeTag] = binop[A]("$times")
  def negate[A: c.WeakTypeTag] = unop[A]("unary_$minus")
  def quot[A: c.WeakTypeTag] = binopSearch[A]("quot" :: "$div" :: Nil) getOrElse failedSearch("quot", "/~")
  def div[A: c.WeakTypeTag] = binop[A]("$div")
  def mod[A: c.WeakTypeTag](stub: => c.Expr[A]) = binop[A]("$percent")
  def equals = binop[Boolean]("$eq$eq")
  def compare = binop[Int]("compare")
}

case class JavaAlgebra[C <: Context](c: C) extends AutoAlgebra {
  def plus[A: c.WeakTypeTag] = 
    binopSearch[A]("add" :: "plus" :: Nil) getOrElse failedSearch("plus", "+")
  def minus[A: c.WeakTypeTag] = 
    binopSearch[A]("subtract" :: "minus" :: Nil) getOrElse failedSearch("minus", "-")
  def times[A: c.WeakTypeTag] = 
    binopSearch[A]("multiply" :: "times" :: Nil) getOrElse failedSearch("times", "*")
  def div[A: c.WeakTypeTag] = 
    binopSearch[A]("divide" :: "div" :: Nil) getOrElse failedSearch("div", "/")
  def negate[A: c.WeakTypeTag] = 
    unopSearch[A]("negate" :: "negative" :: Nil) getOrElse {
      // We can implement negate interms of minus. This is actually required
      // for JScience's Rational :(
      import c.universe._
      c.Expr[A](Apply(
        Select(Ident(newTermName("zero")), newTermName("minus")),
        List(Ident(newTermName("x")))))
    }
  def quot[A: c.WeakTypeTag] =
    binopSearch[A]("quot" :: "divide" :: "div" :: Nil) getOrElse failedSearch("quot", "/~")
  def mod[A: c.WeakTypeTag](stub: => c.Expr[A]) =
    binopSearch("mod" :: "remainder" :: Nil) getOrElse stub

  def equals = binop[Boolean]("equals")
  def compare = binop[Int]("compareTo")
}

object ScalaAutoMacros {
  def semiringImpl[A: c.WeakTypeTag](c: Context): c.Expr[Semiring[A]] =
    ScalaAlgebra[c.type](c).Semiring[A]()

  def rigImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Rig[A]] =
    ScalaAlgebra[c.type](c).Rig[A](z, o)

  def rngImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A]): c.Expr[Rng[A]] =
    ScalaAlgebra[c.type](c).Rng[A](z)

  def ringImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Ring[A]] =
    ScalaAlgebra[c.type](c).Ring[A](z, o)

  def euclideanRingImpl[A: c.WeakTypeTag](c: Context)
      (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[EuclideanRing[A]] =
    ScalaAlgebra[c.type](c).EuclideanRing[A](z, o)(ev)

  def fieldImpl[A: c.WeakTypeTag](c: Context)
      (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[Field[A]] =
    ScalaAlgebra[c.type](c).Field[A](z, o)(ev)

  def eqImpl[A: c.WeakTypeTag](c: Context): c.Expr[Eq[A]] =
    ScalaAlgebra[c.type](c).Eq[A]()

  def orderImpl[A: c.WeakTypeTag](c: Context): c.Expr[Order[A]] =
    ScalaAlgebra[c.type](c).Order[A]()

  def collectionSemigroupImpl[A: c.WeakTypeTag](c: Context): c.Expr[Semigroup[A]] = {
    val ops = ScalaAlgebra[c.type](c)
    c.universe.reify {
      new Semigroup[A] {
        def op(x: A, y: A): A = ops.plusplus[A].splice
      }
    }
  }

  def collectionMonoidImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A]): c.Expr[Monoid[A]] = {
    val ops = ScalaAlgebra[c.type](c)
    c.universe.reify {
      new Monoid[A] {
        def id: A = z.splice
        def op(x: A, y: A): A = ops.plusplus[A].splice
      }
    }
  }
}

object JavaAutoMacros {
  def semiringImpl[A: c.WeakTypeTag](c: Context): c.Expr[Semiring[A]] =
    JavaAlgebra[c.type](c).Semiring[A]()

  def rigImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Rig[A]] =
    JavaAlgebra[c.type](c).Rig[A](z, o)

  def rngImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A]): c.Expr[Rng[A]] =
    JavaAlgebra[c.type](c).Rng[A](z)

  def ringImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Ring[A]] =
    JavaAlgebra[c.type](c).Ring[A](z, o)

  def euclideanRingImpl[A: c.WeakTypeTag](c: Context)
      (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[EuclideanRing[A]] =
    JavaAlgebra[c.type](c).EuclideanRing[A](z, o)(ev)

  def fieldImpl[A: c.WeakTypeTag](c: Context)
      (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[Field[A]] =
    JavaAlgebra[c.type](c).Field[A](z, o)(ev)

  def eqImpl[A: c.WeakTypeTag](c: Context): c.Expr[Eq[A]] =
    JavaAlgebra[c.type](c).Eq[A]()

  def orderImpl[A: c.WeakTypeTag](c: Context): c.Expr[Order[A]] =
    JavaAlgebra[c.type](c).Order[A]()

  def collectionMonoidImpl[A: c.WeakTypeTag](c: Context)(empty: c.Expr[A]): c.Expr[Monoid[A]] = {
    val ops = JavaAlgebra[c.type](c)
    val addx = ops.binop[Unit]("addAll", "z", "x")
    val addy = ops.binop[Unit]("addAll", "z", "y")
    c.universe.reify {
      new Monoid[A] {
        def id: A = empty.splice
        def op(x: A, y: A): A = {
          val z = id
          addx.splice
          addy.splice
          z
        }
      }
    }
  }
}

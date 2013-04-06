package spire.macros

import language.experimental.macros
import scala.reflect.macros.Context

import spire.algebra._

object Auto {
  def semiring[A]() = macro semiringImpl[A]
  def rig[A](z: A, o: A) = macro rigImpl[A]
  def rng[A](z: A) = macro rngImpl[A]
  def ring[A](z: A, o: A) = macro ringImpl[A]
  def euclideanRing[A](z: A, o: A)(implicit ev: Eq[A]) = macro euclideanRingImpl[A]
  def field[A](z: A, o: A)(implicit ev: Eq[A]) = macro fieldImpl[A]

  def eq[A]() = macro eqImpl[A]
  def order[A]() = macro orderImpl[A]

  def semiringImpl[A: c.WeakTypeTag](c: Context)(): c.Expr[Semiring[A]] = {
    val ops = new ScalaOps { val ctx = c }
    c.universe.reify {
      new Semiring[A] {
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
      }
    }
  }

  def rigImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Rig[A]] = {
    val ops = new ScalaOps { val ctx = c }
    c.universe.reify {
      new Rig[A] {
        def zero: A = z.splice
        def one: A = o.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
      }
    }
  }

  def rngImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A]): c.Expr[Rng[A]] = {
    val ops = new ScalaOps { val ctx = c }
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

  def ringImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Ring[A]] = {
    val ops = new ScalaOps { val ctx = c }
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

  def euclideanRingImpl[A: c.WeakTypeTag](c: Context)
      (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[EuclideanRing[A]] = {
    val ops = new ScalaOps { val ctx = c }
    c.universe.reify {
      new EuclideanRing[A] {
        def zero: A = z.splice
        def one: A = o.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
        override def minus(x: A, y: A): A = ops.minus[A].splice
        def negate(x: A): A = ops.negate[A].splice
        def quot(x: A, y: A): A = ops.div[A].splice
        def mod(x: A, y: A): A = ops.mod[A].splice
        def gcd(x: A, y: A): A = euclid(x, y)(ev.splice)
      }
    }
  }

  def fieldImpl[A: c.WeakTypeTag](c: Context)
      (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[Field[A]] = {
    val ops = new ScalaOps { val ctx = c }
    c.universe.reify {
      new Field[A] {
        def zero: A = z.splice
        def one: A = o.splice
        def plus(x: A, y: A): A = ops.plus[A].splice
        def times(x: A, y: A): A = ops.times[A].splice
        override def minus(x: A, y: A): A = ops.minus[A].splice
        def negate(x: A): A = ops.negate[A].splice
        def quot(x: A, y: A): A = ops.div[A].splice
        def mod(x: A, y: A): A = ops.mod[A].splice
        def gcd(x: A, y: A): A = euclid(x, y)(ev.splice)
        def div(x: A, y: A): A = ops.div[A].splice
      }
    }
  }

  def eqImpl[A: c.WeakTypeTag](c: Context)(): c.Expr[Eq[A]] = {
    val ops = new ScalaOps { val ctx = c }
    c.universe.reify {
      new Eq[A] {
        def eqv(x: A, y: A): Boolean = ops.equals.splice
      }
    }
  }

  def orderImpl[A: c.WeakTypeTag](c: Context)(): c.Expr[Order[A]] = {
    val ops = new ScalaOps { val ctx = c }
    c.universe.reify {
      new Order[A] {
        def compare(x: A, y: A): Int = ops.compare.splice
      }
    }
  }

  object collection {
    def semigroup[A]() = macro semigroupImpl[A]
    def monoid[A](z: A) = macro monoidImpl[A]

    def semigroupImpl[A: c.WeakTypeTag](c: Context)(): c.Expr[Semigroup[A]] = {
      val ops = new ScalaOps { val ctx = c }
      c.universe.reify {
        new Semigroup[A] {
          def op(x: A, y: A): A = ops.plusplus[A].splice
        }
      }
    }

    def monoidImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A]): c.Expr[Monoid[A]] = {
      val ops = new ScalaOps { val ctx = c }
      c.universe.reify {
        new Monoid[A] {
          def id: A = z.splice
          def op(x: A, y: A): A = ops.plusplus[A].splice
        }
      }
    }
  }

  object java {
    def semiring[A]() = macro semiringImpl[A]
    def rig[A](z: A, o: A) = macro rigImpl[A]
    def rng[A](z: A) = macro rngImpl[A]
    def ring[A](z: A, o: A) = macro ringImpl[A]
    def euclideanRing[A](z: A, o: A)(implicit ev: Eq[A]) = macro euclideanRingImpl[A]
    def field[A](z: A, o: A)(implicit ev: Eq[A]) = macro fieldImpl[A]

    def eq[A]() = macro eqImpl[A]
    def order[A]() = macro orderImpl[A]

    def semiringImpl[A: c.WeakTypeTag](c: Context)(): c.Expr[Semiring[A]] = {
      val ops = new JavaOps { val ctx = c }
      c.universe.reify {
        new Semiring[A] {
          def plus(x: A, y: A): A = ops.plus[A].splice
          def times(x: A, y: A): A = ops.times[A].splice
        }
      }
    }

    def rigImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Rig[A]] = {
      val ops = new JavaOps { val ctx = c }
      c.universe.reify {
        new Rig[A] {
          def zero: A = z.splice
          def one: A = o.splice
          def plus(x: A, y: A): A = ops.plus[A].splice
          def times(x: A, y: A): A = ops.times[A].splice
        }
      }
    }

    def rngImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A]): c.Expr[Rng[A]] = {
      val ops = new JavaOps { val ctx = c }
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

    def ringImpl[A: c.WeakTypeTag](c: Context)(z: c.Expr[A], o: c.Expr[A]): c.Expr[Ring[A]] = {
      val ops = new JavaOps { val ctx = c }
      println(ops.hasMethod1[A, A, A]("mod"))
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

    def euclideanRingImpl[A: c.WeakTypeTag](c: Context)
        (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[EuclideanRing[A]] = {
      val ops = new JavaOps { val ctx = c }
      c.universe.reify {
        new EuclideanRing[A] {
          def zero: A = z.splice
          def one: A = o.splice
          def plus(x: A, y: A): A = ops.plus[A].splice
          def times(x: A, y: A): A = ops.times[A].splice
          override def minus(x: A, y: A): A = ops.minus[A].splice
          def negate(x: A): A = ops.negate[A].splice
          def quot(x: A, y: A): A = ops.div[A].splice
          def mod(x: A, y: A): A = ops.mod[A].splice
          def gcd(x: A, y: A): A = euclid(x, y)(ev.splice)
        }
      }
    }

    def fieldImpl[A: c.WeakTypeTag](c: Context)
        (z: c.Expr[A], o: c.Expr[A])(ev: c.Expr[Eq[A]]): c.Expr[Field[A]] = {
      val ops = new JavaOps { val ctx = c }
      c.universe.reify {
        new Field[A] {
          def zero: A = z.splice
          def one: A = o.splice
          def plus(x: A, y: A): A = ops.plus[A].splice
          def times(x: A, y: A): A = ops.times[A].splice
          override def minus(x: A, y: A): A = ops.minus[A].splice
          def negate(x: A): A = ops.negate[A].splice
          def quot(x: A, y: A): A = ops.div[A].splice
          def mod(x: A, y: A): A = ops.mod[A].splice
          def gcd(x: A, y: A): A = euclid(x, y)(ev.splice)
          def div(x: A, y: A): A = ops.div[A].splice
        }
      }
    }

    def eqImpl[A: c.WeakTypeTag](c: Context)(): c.Expr[Eq[A]] = {
      val ops = new JavaOps { val ctx = c }
      c.universe.reify {
        new Eq[A] {
          def eqv(x: A, y: A): Boolean = ops.equals.splice
        }
      }
    }

    def orderImpl[A: c.WeakTypeTag](c: Context)(): c.Expr[Order[A]] = {
      val ops = new JavaOps { val ctx = c }
      c.universe.reify {
        new Order[A] {
          def compare(x: A, y: A): Int = ops.compare.splice
        }
      }
    }
  }

  object javaCollection {
    def monoid[A](empty: A) = macro monoidImpl[A]

    def monoidImpl[A: c.WeakTypeTag](c: Context)(empty: c.Expr[A]): c.Expr[Monoid[A]] = {
      val ops = new AutoOps { val ctx = c }
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
}

abstract class AutoOps {
  val ctx: Context
  import ctx.universe._

  def unop[A](name: String, x: String = "x"): ctx.Expr[A] =
    ctx.Expr[A](Select(Ident(newTermName(x)), newTermName(name)))

  def binop[A](name: String, x: String = "x", y: String = "y"): ctx.Expr[A] =
    ctx.Expr[A](Apply(
      Select(Ident(newTermName(x)), newTermName(name)),
      List(Ident(newTermName(y)))))

  def hasMethod1[A: ctx.WeakTypeTag, B: ctx.WeakTypeTag, C: ctx.WeakTypeTag](name: String): Boolean = {
    val tpeA = ctx.weakTypeTag[A].tpe
    val tpeB = ctx.weakTypeTag[B].tpe
    val tpeC = ctx.weakTypeTag[C].tpe

    val matches = for {
      m <- tpeA.members if m.isMethod && m.isPublic && m.name.encoded == name
    } yield {
      m.typeSignature match {
        case MethodType(List(param), ret) =>
          param.typeSignature == tpeB && ret == tpeC
        case _ =>
          false
      }
    }

    matches exists { x => x }
  }
}

abstract class MathOps extends AutoOps {
  def plus[A: ctx.WeakTypeTag]: ctx.Expr[A]
  def minus[A: ctx.WeakTypeTag]: ctx.Expr[A]
  def times[A: ctx.WeakTypeTag]: ctx.Expr[A]
  def negate[A: ctx.WeakTypeTag]: ctx.Expr[A]
  def div[A: ctx.WeakTypeTag]: ctx.Expr[A]
  def mod[A: ctx.WeakTypeTag]: ctx.Expr[A]
  def equals: ctx.Expr[Boolean]
  def compare: ctx.Expr[Int]
}

abstract class ScalaOps extends MathOps {
  def plusplus[A] = binop[A]("$plus$plus")
  def plus[A: ctx.WeakTypeTag] = binop[A]("$plus")
  def minus[A: ctx.WeakTypeTag] = binop[A]("$minus")
  def times[A: ctx.WeakTypeTag] = binop[A]("$times")
  def negate[A: ctx.WeakTypeTag] = unop[A]("unary_$minus")
  def div[A: ctx.WeakTypeTag] = binop[A]("$div")
  def mod[A: ctx.WeakTypeTag] = binop[A]("$percent")
  def equals = binop[Boolean]("$eq$eq")
  def compare = binop[Int]("compare")
}

abstract class JavaOps extends MathOps {
  def plus[A: ctx.WeakTypeTag] = binop[A]("add")
  def minus[A: ctx.WeakTypeTag] = binop[A]("subtract")
  def times[A: ctx.WeakTypeTag] = binop[A]("multiply")
  def div[A: ctx.WeakTypeTag] = binop[A]("divide")
  def negate[A: ctx.WeakTypeTag] = unop[A]("negate")
  def mod[A: ctx.WeakTypeTag] =
    if (hasMethod1[A, A, A]("mod")) {
      binop[A]("mod")
    } else if (hasMethod1[A, A, A]("remainder")) {
      binop[A]("remainder")
    } else {
      ctx.abort(ctx.enclosingPosition,
        "Cannot find suitable mod (%) method (checked: mod, remainder).")
    }
  def equals = binop[Boolean]("equals")
  def compare = binop[Int]("compareTo")
}

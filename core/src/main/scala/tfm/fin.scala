package tfm

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

class fin extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TfmMacro.generateAlgebra
}

object TfmMacro {
  private val SUFFIX = "Interpreter"

  def generateAlgebra(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def verbose(s: => String): Unit =
      if (sys.props.get("tfm.verbose").isDefined) c.info(c.enclosingPosition, s, false)

    // Check that `typeCtor` is paramterized by a unary type constructor
    def wellFormed(typeCtor: ClassDef): Boolean =
      typeCtor.tparams.headOption.filter(_.tparams.size == 1).nonEmpty

    // Compare names of type constructors
    def sameTypeConstructorName(classTparam: Ident, innerTparam: TypeDef): Boolean =
      classTparam.name.decoded == innerTparam.name.decoded

    def generate(algebra: ClassDef, algebraModule: Option[ModuleDef]): c.Expr[Any] = {
      val interpreterType =
        algebra match {
          case q"${mods} trait ${tpname}[..${tparams}] extends { ..${earlydefns} } with ..${parents} { ${self} => ..${stats} }" =>
            tpname
          case _ => c.abort(c.enclosingPosition, "Interpreter must be a trait")
        }

      val effect = algebra.tparams.head

      val algebraName = algebra.name
      val decodedName = algebraName.decoded

      val (algebraType, algebraTerm) = {
        val algebraName =
          if (decodedName.endsWith(SUFFIX)) decodedName.dropRight(SUFFIX.size)
          else c.abort(c.enclosingPosition, "Annottee must end with 'Algebra'")

        (newTypeName(algebraName), newTermName(algebraName))
      }

      val algebras =
        algebra.impl.body.collect {
          // Public defs
          case q"def ${tname}[..${tparams}](...${paramss}): ${outer}[${inner}]" if sameTypeConstructorName(outer.asInstanceOf[Ident], effect) =>
            val valNames = paramss.map(_.map { case ValDef(_, name, _, _) => name })

            q"""
            def ${tname}[..${tparams}](...${paramss}): ${algebraType}[${inner}] =
              new ${algebraType}[${inner}] {
                final def run[F[_]](interpreter: ${interpreterType}[F]): F[${inner}] =
                  interpreter.${tname}[..${tparams}](...${valNames})
              }
            """

          // Public vals
          case q"val ${tname}: ${outer}[${inner}]" if sameTypeConstructorName(outer.asInstanceOf[Ident], effect) =>
            q"""
            val ${tname}: ${algebraType}[${inner}] =
              new ${algebraType}[${inner}] {
                final def run[F[_]](interpreter: ${interpreterType}[F]): F[${inner}] =
                  interpreter.${tname}
              }
            """
        }

      val generatedAlgebra =
        q"""
        trait ${algebraType}[A] {
          def run[F[_]](interpreter: ${interpreterType}[F]): F[A]
        }

        trait Language {
          ..${algebras}
        }

        object language extends Language
        """

      val algebraObject =
        algebraModule match {
          case Some(q"${mods} object ${tname} extends { ..${earlydefns} } with ..${parents} { ${self} => ..${body} }") =>
            q"""${mods} object ${tname} extends { ..${earlydefns} } with ..${parents} { ${self} =>
              ..${generatedAlgebra}
              ..${body}
            }
            """
          case _ =>
            q"""
            object ${algebraName.toTermName} {
              ..${generatedAlgebra}
            }
            """
        }

      verbose {
s"""
Algebras:
---------
${algebras.mkString("\n\n")}

All:
----
${algebra.impl.body.mkString("\n\n")}
"""
      }

      c.Expr(q"""
      ${algebra}

      ${algebraObject}
      """)
    }

    annottees.map(_.tree) match {
      case (algebra: ClassDef) :: Nil if wellFormed(algebra) => generate(algebra, None)
      case (algebra: ClassDef) :: (algebraModule: ModuleDef) :: Nil if wellFormed(algebra) => generate(algebra, Some(algebraModule))
      case _ => c.abort(c.enclosingPosition, "@tfm can only be applied to traits that parameterized with a type constructor")
    }
  }
}

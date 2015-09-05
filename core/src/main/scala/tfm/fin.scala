package tfm

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

class fin[Interpreter[_[_]]] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TfmMacro.generateAlgebra
}

object TfmMacro {
  private val SUFFIX = "Algebra"

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

    val interpreterType =
      c.prefix.tree.collect {
        case q"new fin[$interpreter]" => interpreter
      }.head

    def generate(algebra: ClassDef): c.Expr[Any] = {
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
          case q"def ${name}[..${tparams}](...${vparamss}): ${outer}[${inner}]" if sameTypeConstructorName(outer.asInstanceOf[Ident], effect) =>
            val valNames = vparamss.map(_.map { case ValDef(_, name, _, _) => name })

            q"""
            def ${name}[..${tparams}](...${vparamss}): ${algebraType}[${inner}] =
              new ${algebraType}[${inner}] {
                def run[F[_]](interpreter: ${interpreterType}[F]): F[${inner}] =
                  interpreter.${name}[..${tparams}](...${valNames})
              }
            """

          // Public vals
          case q"val ${name}: ${outer}[${inner}]" if sameTypeConstructorName(outer.asInstanceOf[Ident], effect) =>
            q"""
            val ${name}: ${algebraType}[${inner}] =
              new ${algebraType}[${inner}] {
                def run[F[_]](interpreter: ${interpreterType}[F]): F[${inner}] =
                  interpreter.${name}
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
      $algebra

      object ${algebraName.toTermName} {
        trait ${algebraType}[A] {
          def run[F[_]](interpreter: ${interpreterType}[F]): F[A]
        }

        object ${algebraTerm} {
          ..${algebras}
        }
      }
      """)
    }

    annottees.map(_.tree) match {
      case (algebra: ClassDef) :: Nil if wellFormed(algebra) => generate(algebra)
      case (algebra: ClassDef) :: (_: ModuleDef) :: Nil if wellFormed(algebra) => generate(algebra)
      case _ => c.abort(c.enclosingPosition, "@tfm can only be applied to traits that parameterized with a type constructor")
    }
  }
}

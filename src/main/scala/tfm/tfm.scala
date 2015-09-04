package tfm

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

class tfm[Interpreter[_[_]]] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TfmMacro.generateAlgebra
}

object TfmMacro {
  def generateAlgebra(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    /** Check that `algebra` is paramterized by a unary type constructor */
    def wellFormed(algebra: ClassDef): Boolean =
      algebra.tparams.headOption.filter(_.tparams.size == 1).nonEmpty

    val interpreter =
      c.prefix.tree.collect {
        case q"new tfm[$targ]" => targ
      }.head

    def generate(algebra: ClassDef, algebraCompanion: Option[ModuleDef]): c.Expr[Any] = {
      val suffix = "Algebra"

      val decodedName = algebra.name.decoded

      val algebraName =
        if (decodedName.endsWith(suffix)) decodedName.dropRight(suffix.size)
        else c.abort(c.enclosingPosition, "Annottee must end with 'Algebra'")

      val typeName = newTypeName(algebraName)
      val termName = newTermName(algebraName)

      val algebras =
        algebra.impl.body.collect {
          // Public methods
          case q"def $name[..$tparams](...$vparamss): $outer[$inner]" =>
            val valNames = vparamss.map(_.map { case ValDef(_, name, _, _) => name })

            q"""
            def $name[..$tparams](...$vparamss): $typeName[$inner] =
              new ${typeName}[$inner] {
                def run[F[_]](interpreter: ${interpreter}[F]): F[$inner] =
                  interpreter.${name}[..$tparams](...$valNames)
              }
            """

          // Public values
          case q"val $name: $outer[$inner]" =>
            q"""
            val $name: $typeName[$inner] =
              new ${typeName}[$inner] {
                def run[F[_]](interpreter: ${interpreter}[F]): F[$inner] =
                  interpreter.${name}
              }
            """
        }

      c.Expr(q"""
      $algebra

      trait ${typeName}[A] {
        def run[F[_]](interpreter: ${interpreter}[F]): F[A]
      }

      object ${termName} {
        ..${algebras}
      }
      """)
    }

    annottees.map(_.tree) match {
      case (algebra: ClassDef) :: Nil if wellFormed(algebra) => generate(algebra, None)
      case _ => c.abort(c.enclosingPosition, "@tfm can only be applied to traits that parameterized with a type constructor")
    }
  }
}

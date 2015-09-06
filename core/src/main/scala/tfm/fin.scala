package tfm

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

/** Annotation used to mark a field that should not be treated as part of the algebra.
 *
 *  This becomes useful when you have interpreter-specific methods.
 */
class local extends StaticAnnotation

/** Annotation used to mark a trait containing the algebra.
 *  The trait acts as the interpreter for the algebra.
 *
 *  The trait must be given a name to use for the name of the generated algebra and be
 *  parameterized by a unary type constructor - this type constructor represents the
 *  effect the interpreter needs during interpretation.
 *
 *  The macro will inspect and filter the public fields of the trait and generate
 *  the appropriate algebra type and smart constructors accordingly. When the macro
 *  is invoked, the following is generated and placed in the companion object of
 *  the annottee:
 *
 *  - A trait with the name provided to the annotation. This trait is the user-facing
 *    type - it is the algebra that the interpreter will eventually interpret. The trait
 *    is parameterized by a (proper) type which denotes the type of the value the
 *    underlying expression interprets to.
 *  - A trait named `Language` which contains the smart constructors for the algebra trait.
 *    The smart constructors live in a trait instead of an object to allow potential
 *    library authors to create an object that mixes in the smart constructors, along with
 *    any other thing they may want.
 *  - An object called `language` which extends the generated `Language` trait. This makes
 *    the smart constructors easily available via importing `language._`.
 */
class fin(algebraName: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TfmMacro.generateAlgebra
}

object TfmMacro {
  def generateAlgebra(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    // A predicate used to test if a field could be part of the algebra
    def filterMods(mods: Modifiers): Boolean =
      isPublic(mods) && notOmitted(mods)

    def isPublic(mods: Modifiers): Boolean = {
      val blacklist = List(Flag.PRIVATE, Flag.PROTECTED)
      blacklist.forall(flag => !mods.hasFlag(flag))
    }

    // Field does not contain the `local` annotation
    def notOmitted(mods: Modifiers): Boolean =
      mods.annotations.forall {
        case q"new local()" => false
        case q"new tfm.local()" => false
        case _ => true
      }

    // Verbose output if tfm.verbose system property is set
    def verbose(s: => String): Unit =
      if (sys.props.get("tfm.verbose").isDefined) c.info(c.enclosingPosition, s, false)

    // Check that `clazz` is paramterized by a unary type constructor
    def wellFormed(clazz: ClassDef): Boolean =
      clazz.tparams.headOption.filter(_.tparams.size == 1).nonEmpty

    def generate(interpreter: ClassDef, interpreterModule: Option[ModuleDef]): c.Expr[Any] = {
      // Type parameter of annottee
      val effect = interpreter.tparams.head

      // Identifier has same name as the type parameter of the interpreter
      def isInterpreterEffect(tctor: Ident): Boolean =
        tctor.name.decoded == effect.name.decoded

      // Name of interpreter
      val interpreterType =
        interpreter match {
          case q"${mods} trait ${tpname}[..${tparams}] extends { ..${earlydefns} } with ..${parents} { ${self} => ..${stats} }" =>
            tpname
          case q"${mods} class ${tpname}[..${tparams}] ${ctorMods}(...${paramss}) extends { ..${earlydefns} } with ..${parents} { ${self} => ..${stats} }" =>
            tpname
          case _ => c.abort(c.enclosingPosition, "Interpreter must be a trait or class")
        }

      val interpreterName = interpreter.name
      val decodedInterpreterName = interpreterName.decoded

      // Get first argument of annotation to use as name of algebra
      val (algebraType, algebraTerm) =
        c.prefix.tree match {
          case Apply(_, args) =>
            args.headOption match {
              case Some(name) =>
                val algebraString = c.eval(c.Expr[String](name))

                if (algebraString == decodedInterpreterName)
                  c.abort(c.enclosingPosition, s"Algebra name cannot be same as that of the interpreter: ${decodedInterpreterName}")
                else if (algebraString.isEmpty)
                  c.abort(c.enclosingPosition, "Algebra name cannot be empty string")
                else (newTypeName(algebraString), newTermName(algebraString))
              case None => c.abort(c.enclosingPosition, "Annotation requires algebra name as argument")
            }
        }

      val algebras =
        interpreter.impl.body.collect {
          // Method
          case q"${mods} def ${tname}[..${tparams}](...${paramss}): ${outer}[${inner}]" if filterMods(mods) && isInterpreterEffect(outer.asInstanceOf[Ident]) =>
            val valNames = paramss.map(_.map { case ValDef(_, name, _, _) => name })

            q"""
            def ${tname}[..${tparams}](...${paramss}): ${algebraType}[${inner}] =
              new ${algebraType}[${inner}] {
                final def run[F[_]](interpreter: ${interpreterType}[F]): F[${inner}] =
                  interpreter.${tname}[..${tparams}](...${valNames})
              }
            """

          // Val
          case q"${mods} val ${tname}: ${outer}[${inner}]" if filterMods(mods) && isInterpreterEffect(outer.asInstanceOf[Ident]) =>
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

      val interpreterCompanion =
        interpreterModule match {
          // Existing companion object, extended with the generated algebra
          case Some(q"${mods} object ${tname} extends { ..${earlydefns} } with ..${parents} { ${self} => ..${body} }") =>
            q"""${mods} object ${tname} extends { ..${earlydefns} } with ..${parents} { ${self} =>
              ..${generatedAlgebra}
              ..${body}
            }
            """

          // No companion object, so generate one
          case None =>
            q"""
            object ${interpreterName.toTermName} {
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
${interpreter.impl.body.mkString("\n\n")}
"""
      }

      c.Expr(q"""
      ${interpreter}

      ${interpreterCompanion}
      """)
    }

    annottees.map(_.tree) match {
      case (interpreter: ClassDef) :: Nil if wellFormed(interpreter) =>
        generate(interpreter, None)
      case (interpreter: ClassDef) :: (interpreterModule: ModuleDef) :: Nil if wellFormed(interpreter) =>
        generate(interpreter, Some(interpreterModule))
      case _ =>
        c.abort(c.enclosingPosition, "Annotation can only be applied to traits parameterized with a unary type constructor")
    }
  }
}

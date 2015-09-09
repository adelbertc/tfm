package tfm

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

/** Annotation used to mark a field that should not be treated as part of the algebra.
 *
 *  This becomes useful when you have interpreter-specific methods.
 */
class local extends StaticAnnotation

/** Annotation used to mark a trait or class containing the algebra. Annotation must be
 *  given a name to use as the name of the generated algebra. The annottee should be the
 *  interpreter for the algebra.
 *
 *  The interpreter must be parameterized by a type constructor `F[_]` - this type
 *  constructor represents the effect the interpreter needs during interpretation.
 *  The type constructor may have more than one type parameter.
 *
 *  The macro will inspect and filter the public fields of the interpreter and generate
 *  the appropriate algebra type and smart constructors accordingly. When the macro
 *  is invoked, the following is generated and placed in the companion object of
 *  the annottee:
 *
 *  - A trait with the name provided to the annotation. This trait is the user-facing
 *    type - it is the algebra that the interpreter will eventually interpret. The trait
 *    is parameterized by a (proper) type `A` (or more in the case of higher-arity effects)
 *    which denotes the type of the value the underlying expression interprets to. The
 *    trait contains a single method `run` on it parameterized by a type constructor `G`
 *    with the same shape as the effect, and takes as input an instance of the interpreter
 *    (the annottee) with type `<interpreter type>[G]`. The output of `run` is `G` with
 *    applied to the type parameters of the trait.
 *  - A trait named `Language` which contains the smart constructors for the algebra trait.
 *    The smart constructors share the exact same names of the members of the algebra. The
 *    smart constructors live in a trait instead of an object to allow potential library
 *    authors to create an object that mixes in the smart constructors, along with any other
 *    thing they may want.
 *  - An object called `language` which extends the generated `Language` trait. This makes
 *    the smart constructors easily available via importing `language._`.
 *
 *  The algebra itself has the following restrictions:
 *  - The return type must be of the shape `F[..A]`
 *  - Each input parameter must either be of type with shape `F[..A]` or be of type that does
 *    not contain `F[..A]`. For instance, `Int` and `[A]F[A]` are OK, but `A => F[B]` is not.
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

    // Check that `clazz` is paramterized by a type constructor
    def wellFormed(clazz: ClassDef): Boolean =
      clazz.tparams.map(_.tparams).headOption.toList.flatten.size > 0

    def generate(interpreter: ClassDef, interpreterModule: Option[ModuleDef]): c.Expr[Any] = {
      // Type parameter of annottee
      val _effect@q"${_} type ${effectName}[..${placeholders}] = ${_}" = interpreter.tparams.head
      val outer = _effect.duplicate

      // Type constructor is the same as the interpreter effect
      def isInterpreterEffect(tree: Tree): Boolean = {
        tree match {
          case tq"${outer}[..${inner}]" =>
            outer match {
              case i: Ident if i.name == effectName => true
              case s: Select if s.name == effectName => true
              case _ => inner.exists(isInterpreterEffect)
            }
          case _ => false
        }
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
          case q"${mods} def ${tname}[..${tparams}](...${paramss}): ${_outer}[..${inner}] = ${_}" if filterMods(mods) && isInterpreterEffect(_outer) =>
            // Process params - effectful params are processed differently from pure ones
            val newParamss =
              paramss.map(_.map {
                case q"${mods} val ${uname}: ${outer}[..${inner}] = ${expr}" =>
                  val innerDuplicate = inner.map(_.duplicate)
                  val exprDuplicate = expr.duplicate
                  // Parameter has shape F[A], must interpret parameter before making appropriate interpreter call
                  if (isInterpreterEffect(outer))
                    (q"${mods} val ${uname}: ${algebraType}[..${innerDuplicate}] = ${exprDuplicate}", q"${uname}.run(interpreter)")
                  // Parameter does not contain F[_], e.g. is A, List[String], Double => List[Char]
                  else if (!inner.exists(isInterpreterEffect)) {
                    val v = q"${mods} val ${uname}: ${outer.duplicate}[..${innerDuplicate}] = ${exprDuplicate}"
                    (v, q"${uname}")
                  }
                  // F[_] appears in type of parameter, e.g. A => F[B]
                  else
                    c.abort(c.enclosingPosition, s"Parameter `${tname}: ${outer}[${inner}]` has type containing effect '${effectName.decoded}'")
              })

            val (args, valNames) = (newParamss.map(_.map(_._1)), newParamss.map(_.map(_._2)))
            val innerDuplicate = inner.map(_.duplicate)
            q"""
            def ${tname}[..${tparams}](...${args}): ${algebraType}[..${innerDuplicate}] =
              new ${algebraType}[..${innerDuplicate}] {
                final def run[${outer}](interpreter: ${interpreterName}[${outer.name}]): ${outer.name}[..${innerDuplicate}] =
                  interpreter.${tname}(...${valNames})
              }
            """

          // Val
          case q"${mods} val ${tname}: ${_outer}[..${inner}] = ${_}" if filterMods(mods) && isInterpreterEffect(_outer) =>
            val innerDuplicate = inner.map(_.duplicate)
            q"""
            val ${tname}: ${algebraType}[..${innerDuplicate}] =
              new ${algebraType}[..${innerDuplicate}] {
                final def run[${outer}](interpreter: ${interpreterName}[${outer.name}]): ${outer.name}[..${innerDuplicate}] =
                  interpreter.${tname}
              }
            """
        }

      val inner = placeholders.map(_ => q"type ${newTypeName(c.fresh())}")
      val innerIdent = inner.map(n => Ident(n.name))

      val generatedAlgebra =
        q"""
        trait ${algebraType}[..${inner}] {
          def run[${outer}](interpreter: ${interpreterName}[${outer.name}]): ${outer.name}[..${innerIdent}]
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
        c.abort(c.enclosingPosition, "Annotation can only be applied to types parameterized with a type constructor")
    }
  }
}

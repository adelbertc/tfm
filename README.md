# tfm
tfm is "tagless final macro" - the project is intended to eliminate the boilerplate
associated with setting up an EDSL encoded in the finally tagless approach, specifically
the approach taken in
[this article](https://pchiusano.github.io/2014-05-20/scala-gadts.html).

### Documentation
Currently the documentation is all in the [Scaladoc](core/src/main/scala/tfm/fin.scala).
Examples can be found in the [examples](examples/src/main/scala/tfm/examples) sub-project.

### Limitations
* For algebras with effectful parameters (e.g. have shape `F[_]`) the macro will replace
  each occurence of `F` with the name of the algebra. However, if the `F[_]` appears as
  part of a more complex type (e.g. `A => F[B]`), the macro cannot figure out how to make
  the appropriate interpreter call and will fail.

### Reading
* [Alternatives to GADTs in Scala](https://pchiusano.github.io/2014-05-20/scala-gadts.html)
  * The generated code is taken from this post
* [Typed Tagless Interpretations](http://okmij.org/ftp/tagless-final/index.html)
* [Folding Domain-Specific Languages: Deep and Shallow Embeddings](https://www.cs.ox.ac.uk/publications/publication7584-abstract.html)
* [Combining Deep and Shallow Embedding for EDSL](http://www.cse.chalmers.se/~emax/documents/svenningsson2013combining.pdf)

### License
Code is provided under the MIT license available at http://opensource.org/licenses/MIT, as well as the
LICENSE file.

# Scalazzi compiler plugin

[![Join the chat at https://gitter.im/scalaz/scalaz-plugin](https://badges.gitter.im/scalaz/scalaz-plugin.svg)](https://gitter.im/scalaz/scalaz-plugin?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

See [test/files](plugin/src/test/files) for examples.

Scalazzi Compiler Plugin
 * [x] Sufficiency checker. [Example](plugin/src/test/files/neg/test_bifunctor.scala).
 * [x] Non-ambigious typeclass resolution. [Example](plugin/src/test/files/pos/ambigious_typeclass_parameters.scala).
 * [x] Orphan instance checker. [Example](plugin/src/test/files/neg/orphan_definitions.scala). Still somewhat buggy.

Already provided by other plugins:
 * [x] [Better monadic comprehensions](https://github.com/oleg-py/better-monadic-for).
 * [x] [`@deriving`](https://gitlab.com/fommil/scalaz-deriving/)
 * [x] [`@newtype`](https://github.com/estatico/scala-newtype)

TODO:
 * [ ] require implicits to be either typeclasses or propositions
 * [ ] implicit instantiation in the outermost possible scope (e.g. `Eq[List[A]]` at the same scope as `Eq[A]`)
 * [ ] optimize polymorphic functions by turning `def foo[A]: F[A]` into `val _foo: F[Any] = ...; def foo[A]: F[A] = _foo.asInstanceOf[F[A]]`
 * [ ] simplify functions returning propositions down to a call to `unsafe[TypeParams...]`, which will get optimized into a `val`
 * [ ] `IO` and `ST` fusion
 * [ ] convince Scalac that all instances of a typeclass or a proposition have the same *singleton type*
 * [ ] replace `==` in pattern matching with `===`
 * [ ] convince Scalac that `val x = ...; val y = x` implies `x.type = y.type`
 * [ ] fix exhaustivity checking for newtypes, (`@exhaustive(Just, Empty)`?)

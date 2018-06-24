# Scalazzi compiler plugin

Scalazzi Compiler Plugin
 * [x] sufficiency checker
 * [x] non-ambigious typeclass resolution
 * [x] orphan instance checker

Already provided by other plugins:
 * [x] better monadic comprehensions
 * [x] `@deriving`
 * [x] `@newtype` (might consider integrating it into the plugin)

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

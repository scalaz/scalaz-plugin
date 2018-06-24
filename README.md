# Scalazzi compiler plugin

Scalazzi Compiler Plugin
 * :white_check_mark: sufficiency checker
 * :white_check_mark: non-ambigious typeclass resolution
 * :white_check_mark: orphan instance checker

Already provided by other plugins:
 * :white_check_mark: better monadic comprehensions
 * :white_check_mark: `@deriving`
 * :white_check_mark: `@newtype`

TODO:
 * :x: implicit instantiation in the outermost possible scope (e.g. `Eq[List[A]]` at the same scope as `Eq[A]`)
 * :x: polymorphic values (turn `def foo[A]: F[A]` into `val _foo: F[Any] = ...; def foo[A]: F[A] = _foo.asInstanceOf[F[A]]` automatically)
 * :x: simplify functions returning propositions down to a call to `unsafe[TypeParams...]`, which will get optimized into a `val`
 * :x: `IO` and `ST` fusion
 * :x: convince Scalac that all instances of a typeclass or a proposition have the same *singleton type*
 * :x: replace `==` in pattern matching with `===`
 * :x: convince Scalac that `val x = ...; val y = x` implies `x.type = y.type`
 * :x: fix exhaustivity checking, `@exhaustive(Just, Empty)`
 * :x: require implicits to be either typeclasses or propositions
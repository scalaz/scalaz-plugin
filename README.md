# Scalazzi compiler plugin

[![Join the chat at https://gitter.im/scalaz/scalaz-plugin](https://badges.gitter.im/scalaz/scalaz-plugin.svg)](https://gitter.im/scalaz/scalaz-plugin?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A compiler plugin that improves on the scala compiler on the assumption that the user is using the Scalazzi-safe subset of Scala.

## What does this plugin do?

 * [x] Sufficiency checker. [Example](test/files/neg/test_bifunctor.scala).
 * [x] Non-ambigious typeclass resolution. [Example](test/files/pos/ambigious_typeclass_parameters.scala).
 * [x] Orphan instance checker. [Some](test/files/neg/orphan_definitions.scala) [examples](test/files/neg/eq_contramap.scala).
 * [x] [Polymorphic function optimization](doc/PolymorphicFunctionOptimization.md).

See [test/files](test/files) for more examples. We would like to get a lot more things done eventually, see open issues.

## What is Scalazzi

 * No `null`.
 * No catching exceptions in pure code.
 * No type casing (`isInstanceOf`).
 * No type casting (`asInstanceOf`).
 * No side-effects.
 * No `equals`, `toString`, `hashCode`.
 * No `notify` or `wait` in pure code.
 * No `.getClass`.
 
We are more interested in *Scalazzi+*, which adds:
 * Only total functions.
 * `Eq` (used in laws and pattern matching) must satisfy: 
   + [Identity of indiscernibles](https://en.wikipedia.org/wiki/Identity_of_indiscernibles) (indiscernible values are equal).
   + Indiscernibility of identicals (equal values are indiscernible from each other).

The last requirement might seem a bit cryptic, but it has a wide range of implications, for example:
```scala
if (a === b) {
  // now we know that a.type === b.type
}
```

## Using

```scala
libraryDependencies += "org.scalaz" %% "scalaz-plugin-library" % "77dd9707"
addCompilerPlugin("org.scalaz" % "scalaz-plugin" % "77dd9707" cross CrossVersion.full)
```

---

#### Other great compiler plugins you should check out
 * [Better monadic comprehensions](https://github.com/oleg-py/better-monadic-for).
 * [Automatic typeclass derivation](https://gitlab.com/fommil/scalaz-deriving/).
 * [Newtypes](https://github.com/estatico/scala-newtype).
 * [Automatic code generation for Scala functions and expressions via the Curry-Howard isomorphism](https://github.com/Chymyst/curryhoward).
 * [Mutual tail recursion](https://github.com/wheaties/TwoTails).

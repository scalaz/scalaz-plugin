# Typeclass Mixin Definitions

Expands type class instances to include methods from superclass instances, to avoid boilerplate.

Only acts on instance definitions in objects marked with `@instances`.

Expands:
```scala
case class Identity[A](a: A)

@instances object Identity {
  implicit def identitySemigroup[A](implicit A: Semigroup[A]): Semigroup[Identity[A]] = new Semigroup[Identity[A]] {
    def mappend(a1: Identity[A], a2: => Identity[A]): Identity[A] =
      A.mappend(a1.a, a2.a)
  }

  implicit def identityMonoid[A](implicit A: Monoid[A]): Monoid[Identity[A]] = new Monoid[Identity[A]] {
    val empty: Identity[A] = Identity(A.empty)
  }
}
```

into:

```scala
case class Identity[A](a: A)

object Identity {
  implicit def identitySemigroup[A](implicit A: Semigroup[A]): Semigroup[Identity[A]] = new Semigroup[Identity[A]] {
    def mappend(a1: Identity[A], a2: => Identity[A]): Identity[A] =
      A.mappend(a1.a, a2.a)
  }

  implicit def identityMonoid[A](implicit A: Monoid[A]): Monoid[Identity[A]] = new Monoid[Identity[A]] {
    def mappend(a1: Identity[A], a2: => Identity[A]): Identity[A] =
      A.mappend(a1.a, a2.a)
    val empty: Identity[A] = Identity(A.empty)
  }
}
```

Copies code *verbatim* from superclass instances, to avoid forcing the user to find ways to factor the code. No variable renaming or anything;
if you use a parameter called `A` as a `Semigroup[A]` in your superclass instance definition, there had better be a value `A` in the subclass
instance's parameters which can be typed as `Semigroup[A]`.

Scalaz 7.2.x and cats have gigantic amounts of boilerplate to declare trait hierarchies that factor type class instance code; this plugin does it for you.

Note: Implicits are filled in *before* this expansion occurs, so make sure to order context bounds consistently.

## Disabling code copying

Disabling code copying can be done on the instance-level by marking definitions with `@unmixin`, e.g.:

```scala
case class Identity[A](a: A)

object Identity {
  @unmixin implicit def identitySemigroup[A](implicit A: Semigroup[A]): Semigroup[Identity[A]] = new Semigroup[Identity[A]] {
    def mappend(a1: Identity[A], a2: => Identity[A]): Identity[A] =
      A.mappend(a1.a, a2.a)
  }
}
```

Instances marked `@unmixin` will not only not have code copied into them from superclass instances, but their code will not be copied into *other*
instance definitions.

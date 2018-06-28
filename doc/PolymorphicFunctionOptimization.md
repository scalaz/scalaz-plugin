# Polymorphic Function Optimization

In short, any polymorphic function that has no value paramaters and is
*modifiable* in a *static scope*, such as
```scala
def foo[A, B[_]]: C[A, B] = body
```
can be optimized to
```scala
val foo$1: C[Any, Any] = body.asInstanceOf[C[Any, Any]]
def foo[A, B[_]]: C[A, B] = foo$1.asInstanceOf[C[A, B]]
```

#### Requirements
 * Parametricity, as follows from erasure.
 * No side-effects.
 * No observable reference equality.

## Static scopes
We define static scope as any class or object scope such that it is
reachable without holding any references to any classes or objects.

```scala
object A {
  // static scope
  object B {
    // static scope
  }
  class C {
    // not static scope, requires a reference to c : C
    object D {
      // not static scope, requires a reference to c : C
    }
  }
  val c = new C {
    // static scope
  }
  // while strictly speaking we don't define a new class here,
  // but since C is not a final class, we consider the scope
  // inside C to be static
  val d = new C
}

package object E {
  // static scope
}
```

## Modifiable definitions

We consider any non-final definition declared in a class or object or
in any of its parents to be modifiable in the class or object:
```scala
trait A {
  def a : Int
}
object B extends A {
  // modifiable in A
  def b: Int
  val c: Int
  // also modifiable in A, even if not defined here
  // def a : Int
}
```

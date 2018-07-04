import scalaz.meta.Typeclass

trait resolution_many_choices {
  trait Functor[F[_]] extends Typeclass {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  trait Applicative[F[_]] extends Functor[F] {
    def unit: F[Unit]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[A]]
  }

  object Foo {
    implicit def foo[F[_]]: Traverse[F] = ???
    object Baz {
      implicit def baz[F[_]]: Traverse[F] = ???
    }

    class Boo[F[_]] {
      import Baz._
      implicit def boo[F[_]]: Traverse[F] = ???

      // Ambigious instances for typeclasses are okay!
      def test[F[_]: Traverse: Applicative, A](fa: F[A]): F[Unit] =
        implicitly[Functor[F]].map(fa)(_ => ())
    }
  }
}

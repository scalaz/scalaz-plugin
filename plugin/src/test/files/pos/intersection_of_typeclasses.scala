import scalaz.meta.{ minimal, Typeclass }

trait Functor[F[_]] extends Typeclass {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
@minimal(("unit", "product", "map"), ("pure", "ap"))
trait Applicative[F[_]] extends Functor[F] {
  def unit: F[Unit] =
    pure(())
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap[B, (A, B)](ap[A, B => (A, B)](pure(a => b => (a, b)))(fa))(fb)
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)

  def pure[A](a: A): F[A] =
    map(unit)(_ => a)
  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map(product(fab, fa)) { case (f, a) => f(a) }
}

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

final case class Id[A](value: A)
object Id {
  implicit def instance: Applicative[Id] with Traverse[Id] =
    new Applicative[Id] with Traverse[Id] {
      override def map[A, B](fa: Id[A])(f: A => B): Id[B] =
        Id(f(fa.value))
      override def unit: Id[Unit] =
        Id(())
      override def product[A, B](fa: Id[A], fb: Id[B]): Id[(A, B)] =
        Id((fa.value, fb.value))
      override def traverse[G[_], A, B](fa: Id[A])(f: A => G[B])(implicit G: Applicative[G]): G[Id[B]] =
        G.map(f(fa.value))(Id.apply)
    }
}

object Test {
  def test[F[_]: Traverse: Applicative, A](fa: F[A]): F[Unit] =
    implicitly[Functor[F]].map(fa)(_ => ())

  val x = test(Id(1))
}

import scalaz.meta.{instances, Typeclass}

trait F[A] extends Typeclass {
  def dummy: A
}

trait G[A] extends F[A]

trait H[A] extends G[A]

case class Identity[A](a: A)

@instances object Identity {
  implicit def h[A]: H[Identity[A]] =
    new H[Identity[A]] {}
}

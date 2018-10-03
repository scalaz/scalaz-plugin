import scalaz.meta.{instances, unmixin, Typeclass}

trait F[A] extends Typeclass {
  def dummy: A
}

case class Identity[A](a: A)

@instances object Identity {
  implicit def f1[A]: F[Identity[A]] =
    new F[Identity[A]] {}

  implicit def f2[A]: F[Identity[A]] =
    new F[Identity[A]] {}

  implicit def f3[A]: F[Identity[A]] =
    new F[Identity[A]] {}

  @unmixin implicit def f4[A]: F[Identity[A]] =
    new F[Identity[A]] {
      def dummy = ???
    }
}

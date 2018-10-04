import scalaz.meta.{instances, unmixin, Typeclass}

trait F[A] extends Typeclass

case class Identity[A](a: A)

@instances object Identity {
  implicit def f1[A]: F[Identity[A]] = {
    val x = 1
    new F[Identity[A]] {}
  }
  @unmixin implicit def f4[A]: F[Identity[A]] = {
    val x = 1
    new F[Identity[A]] {}
  }
}

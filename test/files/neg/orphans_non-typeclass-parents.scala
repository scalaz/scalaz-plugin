import scalaz.meta.Typeclass

trait Functor[F[_]] extends Typeclass
trait Blah

object Functor {
  implicit val instance: Functor[Option] = new Functor[Option] with Blah { }
}
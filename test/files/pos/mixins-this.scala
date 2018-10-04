import scalaz.meta.{instances, Typeclass}

trait Semigroup[A] extends Typeclass {
  def mappend(fst: A, snd: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def mempty: A
}

trait CommutativeMonoid[A] extends Monoid[A] {
  def mempty2: A
}

case class Identity[A](a: A)

@instances object Identity {
  implicit def semigroup[A](implicit A: Semigroup[A]): Semigroup[Identity[A]] = new Semigroup[Identity[A]] {
    def mappend(fst: Identity[A], snd: Identity[A]): Identity[A] = Identity(A.mappend(fst.a, snd.a))
  }

  implicit def monoid[A](implicit A: Monoid[A]): Monoid[Identity[A]] = new Monoid[Identity[A]] {
    def mempty: Identity[A] = Identity[A](A.mempty)
  }

  implicit def commutativeMonoid[A](implicit A: CommutativeMonoid[A]): CommutativeMonoid[Identity[A]] =
    new CommutativeMonoid[Identity[A]] {
      def mempty2 = {
        this.mempty
        mempty
      }
    }
}

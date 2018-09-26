import scalaz.meta.{instances, unmixin, Typeclass}

trait Semigroup[A] extends Typeclass {
  def mappend(fst: A, snd: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def mempty: A
}

@instances object Identity {
  @unmixin implicit val semigroup: Semigroup[Int] = new Semigroup[Int] {
    def mappend(fst: Int, snd: Int): Int = fst + snd
  }

  @unmixin implicit val monoid: Monoid[Int] = new Monoid[Int] {
    def mappend(fst: Int, snd: Int): Int = fst + snd
    def mempty: Int = 0
  }
}

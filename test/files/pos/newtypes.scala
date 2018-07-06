import scalaz.meta.newtype

package object foo {

  @newtype class PosInt(private val value: Int) {

  }
  object PosInt {
    def apply(i: Int): Option[PosInt] =
      if (i > 0) Some(i.asInstanceOf[PosInt])
      else None
  }

  val i: PosInt = PosInt(10).get
}
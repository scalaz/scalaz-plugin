trait DisjunctionFunctions
trait MaybeFunctions {
  def empty[A]: Option[A]      = None
}

trait AllFunctions extends DisjunctionFunctions with MaybeFunctions
object Scalaz extends AllFunctions {
  type Test = String
  override def empty[A]: Option[A] = super.empty
}
trait DisjunctionFunctions
trait MaybeFunctions {
  def empty[A]: Option[A]      = None
}

trait AllFunctions extends DisjunctionFunctions with MaybeFunctions
object Scalaz extends AllFunctions
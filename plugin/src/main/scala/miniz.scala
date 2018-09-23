package miniz

final case class NonEmptyList[+A](head: A, tail: List[A]) {
  def mkString(sep: String): String = head.toString + sep + tail.mkString(sep)
}

object `package` {
  type \/[+A, +B] = A Either B
  val -\/ : Left.type  = Left
  val \/- : Right.type = Right
  implicit class either_ops[A, B](val self: A \/ B) {
    def leftMap[C](f: A => C): C \/ B = self match {
      case \/-(b) => \/-(b)
      case -\/(a) => -\/(f(a))
    }
  }

  type /\[+A, +B] = (A, B)
  val /\ : Tuple2.type = Tuple2
  implicit class ToExtraTuple2Ops[A](val a: A) {
    def /\[B](b: B): (A, B) = (a, b)
  }

  val * : Unit = ()

  implicit class ToExtraListOps[A](list: List[A]) {
    def toNel: Option[NonEmptyList[A]] = list match {
      case Nil => None
      case x :: xs => Some(NonEmptyList(x, xs))
    }
  }

  implicit class ToExtraListEitherOps[E, A](list: List[Either[E, A]]) {
    def separate: (List[E], List[A]) =
      (list.flatMap(_.left.toOption),
        list.flatMap(_.right.toOption))
  }
}

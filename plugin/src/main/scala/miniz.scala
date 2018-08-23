package miniz

import scala.annotation.tailrec

object `package` {
  trait Tag extends Any
  type Base

  type \/[+A, +B] = A Either B
  val -\/ : Left.type  = Left
  val \/- : Right.type = Right
  implicit class either_ops[A, B](val self: A \/ B) extends AnyVal {
    def leftMap[C](f: A => C): C \/ B = self match {
      case \/-(b) => \/-(b)
      case -\/(a) => -\/(f(a))
    }
  }

  type /\[+A, +B] = (A, B)
  val /\ : Tuple2.type = Tuple2
  implicit class tuple2_ops[A](val a: A) extends AnyVal {
    def /\[B](b: B): (A, B) = (a, b)
  }

  type NonEmptyList[+A] <: Base with Tag
  implicit class nel_ops[A](val self: NonEmptyList[A]) extends AnyVal {
    def toList: List[A] = self.asInstanceOf[List[A]]

    def uncons: (A, List[A]) = (toList.head, toList.tail)
    def head: A              = toList.head
    def tail: List[A]        = toList.tail

    def ::(a: A): NonEmptyList[A] = NonEmptyList(a, toList)
    def +:(a: A): NonEmptyList[A] = a :: self
    def :+(a: A): NonEmptyList[A] = (toList :+ a).asInstanceOf[NonEmptyList[A]]

    def map[B](f: A => B): NonEmptyList[B] =
      toList.map(f).asInstanceOf[NonEmptyList[B]]

    def foldRight[Z](one: A => Z)(more: (A, => Z) => Z): Z =
      toList match {
        case a :: Nil => one(a)
        case a :: as  => more(a, as.asInstanceOf[NonEmptyList[A]].foldRight(one)(more))
      }

    def foldLeft[Z](one: A => Z)(more: (Z, A) => Z): Z = {
      @tailrec def go(z: Z, l: List[A]): Z = l match {
        case a :: as => go(more(z, a), as)
        case Nil     => z
      }

      self.toList match {
        case a :: as => go(one(a), as)
      }
    }
  }
  object NonEmptyList {
    def apply[A](head: A, tail: List[A]): NonEmptyList[A] =
      (head :: tail).asInstanceOf[NonEmptyList[A]]
    def of[A](head: A, rest: A*): NonEmptyList[A] =
      List(head +: rest: _*).asInstanceOf[NonEmptyList[A]]
  }
}

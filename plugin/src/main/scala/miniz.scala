package miniz

import scala.annotation.tailrec

final case class Nel[+A](head: A, tail: List[A]) {
  import Nel._

  def ::[B >: A](b: B): Nel[B] = Nel(b, toList)
  def +:[B >: A](b: B): Nel[B] = b :: this
  def :+[B >: A](b: B): Nel[B] = Nel(head, tail :+ b)

  def toList: List[A] = head :: tail
  def prependList[B >: A](list: List[B]): Nel[B] = list match {
    case Nil     => this
    case b :: bs => Nel(b, bs ++ (head :: tail))
  }

  def map[B](f: A => B): Nel[B] =
    Nel(f(head), tail.map(f))

  def foldRight[Z](one: A => Z)(more: (A, => Z) => Z): Z =
    tail.toNel match {
      case None    => one(head)
      case Some(l) => more(head, l.foldRight(one)(more))
    }

  def foldLeft[Z](one: A => Z)(more: (Z, A) => Z): Z = {
    @tailrec def go(z: Z, l: List[A]): Z = l match {
      case a :: as => go(more(z, a), as)
      case Nil     => z
    }
    go(one(head), tail)
  }

  def foldRightH[Z](
    one: (List[A], A) => Z,
    more: (List[A], Nel[A], A, => Z) => Z
  ): Z = {
    def go(nel: Nel[A], ctx: List[A]): Z = nel match {
      case Nel(h, Nil) => one(ctx, h)
      case Nel(h, t :: ts) =>
        val rest = Nel(t, ts)
        more(ctx, rest, h, go(rest, h :: ctx))
    }

    go(this, Nil)
  }

  def foldRightEitherH[E, Z](
    one: (List[A], A) => E \/ Z,
    more: (List[A], Nel[A], A, => Z) => E \/ Z
  ): E \/ Z = {
    def go(nel: Nel[A], ctx: List[A]): E \/ Z = nel match {
      case Nel(h, Nil) =>
        one(ctx, h)

      case Nel(h, t :: ts) =>
        val rest = Nel(t, ts)

        go(rest, h :: ctx) match {
          case Left(e)  => Left(e)
          case Right(x) => more(ctx, rest, h, x)
        }
    }

    go(this, Nil)
  }

  def mkString(sep: String): String = head.toString + sep + tail.mkString(sep)
}
object Nel {
  def of[A](head: A, rest: A*): Nel[A] =
    Nel(head, rest.toList)
}

object `package` {
  type \/[+A, +B] = A Either B
  val -\/ : Left.type  = Left
  val \/- : Right.type = Right
  implicit class ToEitherOps[A, B](val self: A \/ B) extends AnyVal {
    def leftMap[C](f: A => C): C \/ B = self match {
      case \/-(b) => \/-(b)
      case -\/(a) => -\/(f(a))
    }
  }

  type /\[+A, +B] = (A, B)
  val /\ : Tuple2.type = Tuple2
  implicit class ToExtraTuple2Ops[A](val a: A) extends AnyVal {
    def /\[B](b: B): (A, B) = (a, b)
  }

  val * : Unit = ()

  implicit class ToExtraListOps[A](val list: List[A]) extends AnyVal {
    def toNel: Option[Nel[A]] = list match {
      case Nil     => None
      case x :: xs => Some(Nel(x, xs))
    }

    def foldRightH[Z](
      nil: List[A] => Z,
      cons: (List[A], List[A], A, => Z) => Z
    ): Z = {
      def go(value: List[A], ctx: List[A]): Z = value match {
        case Nil    => nil(ctx)
        case h :: t => cons(ctx, t, h, go(t, h :: ctx))
      }

      go(list, Nil)
    }
  }

  implicit class ToExtraListEitherOps[E, A](val list: List[Either[E, A]]) extends AnyVal {
    def separate: (List[E], List[A]) =
      (list.flatMap(_.left.toOption), list.flatMap(_.right.toOption))
  }
}

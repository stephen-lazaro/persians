package sl.persians

import cats.{Functor, Semigroup}
import cats.data.Ior

trait Align[F[_]] extends Functor[F] {
  def nil[A]: F[A]
  def align[A, B](fa: F[A], fb: F[B]): F[Ior[A, B]]

  def alignWith[A, B, C](fa: F[A], fb: F[B])(f: Ior[A, B] => C): F[C] =
    map(align(fa, fb))(f)

  // Could be done with option monoid syntax lifting the semigroup
  def combineAlign[A: Semigroup](fa: F[A], faa: F[A]): F[A] =
    alignWith(fa, faa)({
      case Ior.Right(b) => b
      case Ior.Left(a)  => a
      case Ior.Both(a, b) =>
        Semigroup[A].combine(a, b)
    })

  def padZip[A, B](fa: F[A], fb: F[B]): F[(Option[A], Option[B])] =
    alignWith(fa, fb)({
      case Ior.Right(b) => (None, Some(b))
      case Ior.Left(a)  => (Some(a), None)
      case Ior.Both(a, b) => (Some(a), Some(b))
    })
}
object Align {
  def apply[F[_]](implicit F: Align[F]): Align[F] = F

  implicit val alignForList: Align[List] = new Align[List] {
    def nil[A]: List[A] = List.empty[A]

    def align[A, B](fa: List[A], fb: List[B]): List[Ior[A, B]] = {
      def go(la: List[A], lb: List[B]): List[Ior[A, B]] =
        (la.headOption, lb.headOption) match {
          case (Some(a), Some(b)) => Ior.Both(a, b) :: go(la.tail, lb.tail)
          case (None, Some(b))    => Ior.Right(b)   :: go(Nil, lb.tail)
          case (Some(a), None)    => Ior.Left(a)    :: go(la.tail, Nil)
        }

      go(fa, fb)
    }
  }

  implicit val alignForVector: Align[Vector] = new Align[Vector] {
    def nil[A]: Vector[A] = Vector.empty[A]

    def align[A, B](fa: Vector[A], fb: Vector[B]): Vector[Ior[A, B]] = {
      def go(la: Vector[A], lb: Vector[B]): Vector[Ior[A, B]] =
        (la.headOption, lb.headOption) match {
          case (Some(a), Some(b)) => Ior.Both(a, b) +: go(la.tail, lb.tail)
          case (None, Some(b))    => Ior.Right(b)   +: go(Vector.empty, lb.tail)
          case (Some(a), None)    => Ior.Left(a)    +: go(la.tail, Vector.empty)
        }

      go(fa, fb)
    }
  }

  implicit val alignForOption: Align[Option] = new Align[Option] {
    def nil[A]: Option[A] = None

    def align[A, B](fa: Option[A], fb: Option[B]): Option[Ior[A, B]] =
      (fa, fb) match {
        case (None, None)       => None
        case (Some(a), None)    => Option(Ior.Left(a))
        case (None, Some(b))    => Option(Ior.Right(b))
        case (Some(a), Some(b)) => Option(Ior.Both(a, b))
      }
  }
}

package sl

import cats.{Functor, Id}
import cats.instances.function.catsStdInstancesForFunction1
import cats.syntax.arrow.toArrowOps

package object persians {
  type Co[W[_], A] = CoT[W, Id, A]
  type SelfDual[F[_]] = Dual[F, F]

  def uncozipLeft[F[_]: Functor, A, B](feither: Either[F[A], F[B]]): F[Either[A, B]] =
    feither.fold[F[Either[A, B]]](
      Functor[F].lift(Left.apply[A, B]),
      Functor[F].lift(Right.apply[A, B])
    )

  def unzipRight[F[_]: Functor, A, B]: F[(A, B)] => (F[A], F[B]) =
    Functor[F].lift[(A, B), A](_._1) &&& Functor[F].lift[(A, B), B](_._2)

  def unsplitLeft[F[_]: Functor, A](a: A)(funit: F[Unit]): F[A] =
    Functor[F].as(funit, a)
}

package sl.persians

import cats.Functor
import cats.data.Ior

trait Align[F[_]] extends Functor[F] {
  def nil[A]: F[A]
  def align[A, B](fa: F[A], fb: F[B]): F[Ior[A, B]]

  def alignWith[A, B, C](fa: F[A], fb: F[B])(f: Ior[A, B] => C): F[C] =
    map(align(fa, fb))(f)
}
object Align {
  def apply[F[_]](implicit F: Align[F]): Align[F] = F
}

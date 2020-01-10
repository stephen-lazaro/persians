package sl.persians

import cats.{Bifunctor, Bifoldable}

trait Bicrosswalk[F[_, _]] extends Bifunctor[F] with Bifoldable[F] {
  def bicrosswalk[G[_]: Align, A, B, C, D](fab: F[A, B])(f: A => G[C])(g: B => G[D]): G[F[C, D]]

  def bisequenceL[G[_]: Align, A, B](fgab: F[G[A], G[B]]): G[F[A, B]] =
    bicrosswalk(fgab)(identity)(identity)
}
object Bicrosswalk {
  def apply[F[_, _]](implicit F: Bicrosswalk[F]): Bicrosswalk[F] = F
}

package sl.persians

import cats.{Foldable, Functor}

trait Crosswalk[F[_]] extends Functor[F] with Foldable[F] {
 def crossWalk[G[_]: Align, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
 def sequenceL[G[_]: Align, A](fga: F[G[A]]): G[F[A]] = crossWalk(fga)(identity)
}
object Crosswalk {
  def apply[F[_]](implicit F: Crosswalk[F]): Crosswalk[F] = F
}

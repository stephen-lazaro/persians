package sl.persians

import cats.data.Ior

trait Unalign[F[_]] extends Align[F] {
  def unalign[A, B](fab: F[Ior[A, B]]): (F[Option[A]], F[Option[B]])
}
object Unalign {
  def apply[F[_]](implicit F: Unalign[F]): Unalign[F] = F
}

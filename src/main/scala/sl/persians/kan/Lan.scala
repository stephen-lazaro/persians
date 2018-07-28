package sl.persians.kan

import cats.{~>, Functor}

trait Lan [G [_], H [_], A] {
  type B
  def run: (G[B] => A, H[B])
}
object Lan {
  def toLan [F [_]: Functor, H [_], G [_], B] (trans: H ~> λ[A => F [G [A]]])(lgh: Lan[G, H, B]): F[B] =
    lgh.run match {
      case (f, v) =>
        Functor[F].map(trans.apply[lgh.B](v))(f)
    }

  def apply [G [_], H [_], A] (ha: H[A]): Lan[G, H, G[A]] = new Lan[G, H, G[A]] {
    type B = A
    def run: (G[this.B] => G[A], H[this.B]) = (identity[G[A]], ha)
  }

  def fromLan [F[_], G[_], H[_], B] (trans: Lan[G, H, ?] ~> F)(hb: H[B]): F[G[B]] = trans.apply[G[B]](Lan.apply[G, H, B](hb))


  implicit def functorLan[F[_], G[_]]: Functor[Lan[F, G, ?]] =
    new Functor[Lan[F, G, ?]] {
      def map [A, C] (fa: Lan[F, G, A])(f: A => C): Lan[F, G, C] =
        new Lan[F, G, C] {
          type B = fa.B
          def run: (F[B] => C, G[B]) = fa.run match {
            case (g, ha) => (g andThen f, ha)
          }
        }
    }
}

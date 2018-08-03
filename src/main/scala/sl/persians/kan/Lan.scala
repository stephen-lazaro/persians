package sl.persians.kan

import cats.{~>, Functor, Id}
import cats.free.Coyoneda

import sl.persians.Density

trait Lan [G [_], H [_], A] {
  type B
  def run: (G[B] => A, H[B])
  def fi: H[B] = run._2
  def k: G[B] => A = run._1
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

  def toCoyoneda[F[_], A](lan: Lan[Id, F, A]): Coyoneda[F, A] =
    Coyoneda.apply[F, lan.B, A](lan.fi)(lan.k)

  def fromCoyoneda[F[_], A](coyoneda: Coyoneda[F, A]): Lan[Id, F, A] =
    new Lan[Id, F, A] {
      type B = coyoneda.Pivot
      def run = (coyoneda.k, coyoneda.fi)
    }

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

  def toDensity[F[_], A](lan: Lan[F, F, A]): Density[F, A] =
    new Density[F, A] {
      type B = lan.B
      def run: (F[B] => A, F[B]) = lan.run
    }
}

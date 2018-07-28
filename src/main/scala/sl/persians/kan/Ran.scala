package sl.persians.kan

import cats.{~>, Functor}

trait Ran [G [_], H [_], A] {
  def run [B] (given: A => G [B]): H [B]
}
object Ran {
  def toRan [F [_]: Functor, G [_], H [_], B] (trans: λ[A => F [G [A]]] ~> H)(fb: F[B]): Ran[G, H, B] =
    new Ran [G, H, B] {
      def run [C] (given: B => G[C]): H[C] = trans.apply[C](Functor[F].map(fb)(given))
    }

  def fromRan [F[_], G[_], H[_], B](trans: F ~> Ran[G, H, ?])(fgb: F[G[B]]): H[B] =
    trans.apply[G[B]](fgb).run(identity[G[B]])

  implicit def functorForRan[G[_], H[_]]: Functor[Ran[G, H, ?]] =
    new Functor[Ran[G, H, ?]] {
      def map[A, B](fa: Ran[G, H, A])(f: A => B): Ran[G, H, B] =
        new Ran[G, H, B] {
          def run [C] (given: B => G[C]): H[C] =
           fa.run[C](given compose f)
        }
    }
}

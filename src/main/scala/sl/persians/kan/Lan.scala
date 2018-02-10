package sl.persians.kan

import cats.{~>, Functor}

trait Lan [G [_], H [_], A]{
  def run [B] (a: G [B] => A): H [B]
}
object Lan {
  def toLan [F [_]: Functor, H [_], G [_], B] (trans: H ~> λ[A => F [G [A]]]) =
    new Lan [G, H, B => F [B]] {
      def run [C] (given: G [B => F [B]] => C): H [B => F [B]] =
        given andThen (_ map trans.apply [C])
    }
}

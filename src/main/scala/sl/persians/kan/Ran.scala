package sl.persians.kan

//import cats.{~>, Functor}

trait Ran [G [_], H [_], A] {
  def run [B] (a: A => G [B]): H [B]
}
object Ran {
  //def toRan [F [_]: Functor, H [_], G [_], B] (trans: H ~> λ[A => F [G [A]]]): Ran[G, H, B => F[B]] =
   // new Ran [G, H, B => F[B]] {
   //   def run [C] (given: A => G[B => F[C]]) = ???
    //}
}

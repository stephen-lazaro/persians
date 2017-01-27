package sl.kaaan

import cats.Functor
import shapeless.PolyDefns.~>

/**
  * The Yoneda lemma says that for a set valued functor F
  * from a category with small Hom-sets the values for
  * an object r in the source category just are
  * natural transformations from the Hom(r, -) functor
  * to F(r).
  *
  * In other words, natural transformations to functions
  * are in bijection with the values of the functor.
  *
  * Looking at type signatures, you'll see that Yoneda
  * as a datatype curries a functor on the right.
  */
trait Yoneda [F [_], A] {
  def roYo: λ [B => (A => B)] ~> F
  def run = roYo
}
object Yoneda {
  def liftYoneda [F [_]: Functor, A] (fa: F[A]): Yoneda [F, A] = new Yoneda [F, A] {
    override def roYo = new (λ [B => (A => B)] ~> F) {
      override def apply [T] (f: A => T) = implicitly [Functor [F]].map (fa)(f)
    }
  }
  def lowerYoneda [F [_]: Functor, A] (ya: Yoneda [F, A]): F [A] =
    ya roYo identity [A]
}

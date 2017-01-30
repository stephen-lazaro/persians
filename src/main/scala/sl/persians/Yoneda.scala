package sl.persians

import cats.{Apply, Functor}
import cats.syntax.apply._
import shapeless.PolyDefns.~>

/**
  * The (covariant) Yoneda lemma says that for a set valued
  * functor F from a category with small Hom-sets the values
  * for an object r in the source category just are
  * natural transformations from the Hom(r, -) functor
  * to F(r).
  *
  * In other words, natural transformations to functions
  * are in bijection with the values of the functor.
  *
  * Looking at type signatures, you'll see that Yoneda
  * as a datatype curries a functor's map on the value side.
  *
  * This is in line with the proof of the Yoneda lemma
  * which amounts to showing that liftYoneda and
  * lowerYoneda really do work, in other words
  * that values are determined by lifting the identity
  * across our natural transformation.
  */
trait Yoneda [F [_], A] {
  def roYo: λ [B => (A => B)] ~> F
  def run = roYo
}
object Yoneda {
  def liftYoneda [F [_]: Functor, A] (fa: F[A]): Yoneda [F, A] = new Yoneda [F, A] {
    def roYo = new (λ [B => (A => B)] ~> F) {
      def apply [T] (f: A => T) = implicitly [Functor [F]] .map (fa)(f)
    }
  }
  def upYo [F [_] : Functor, A] (fa: F [A]) = liftYoneda [F, A] (fa)
  def yUp  [F [_] : Functor, A] (fa: F [A]) = liftYoneda [F, A] (fa)

  def lowerYoneda [F [_]: Functor, A] (ya: Yoneda [F, A]): F [A] =
    ya roYo identity [A] _
  // Apparently this can't be eta reduced, though I am not sure why
  def loYo [F [_] : Functor, A] (ya: Yoneda [F, A]) = lowerYoneda [F, A] (ya)
  def yoLo [F [_] : Functor, A] (ya: Yoneda [F, A]) = lowerYoneda [F, A] (ya)

  object instances {
    implicit def stdFunctorForYoneda [F [_]] = new Functor [Yoneda [F, ?]] {
      def map [A, B] (fa: Yoneda [F, A])(f: A => B) =
        new Yoneda [F, B] {
          def roYo = new (λ [C => (B => C)] ~> F) {
            def apply [T] (k: B => T) = fa roYo (k compose f)
          }
        }
    }
    implicit class functorSyntaxForYoneda [F [_], A] (ya: Yoneda [F, A]) {
      def map [B] (f: A => B) = stdFunctorForYoneda.map [A, B] (ya)(f)
    }

    implicit def stdApplyForYoneda [F [_] : Apply] = new Apply [Yoneda [F, ?]] {
      def map [A, B] (fa: Yoneda [F, A])(f: A => B) = stdFunctorForYoneda.map(fa)(f)
      def ap [A, B] (ff: Yoneda [F, A => B])(ya: Yoneda [F, A]): Yoneda [F, B] =
        new Yoneda [F, B] {
          def roYo = new (λ [C => (B => C)] ~> F) {
            def apply [T] (k: B => T) =
              (Yoneda loYo [F, A => T] (ff map (k compose _))) ap (
                Yoneda loYo [F, A] ya)
          }
        }
    }
  }
}

package sl.persians

import cats.{Applicative, Apply, Functor}
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
  * across each given natural transformation.
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

  def lowerYoneda [F [_], A] (ya: Yoneda [F, A]): F [A] =
    ya roYo identity [A] _
  // Apparently this can't be eta reduced, though I am not sure why
  def loYo [F [_], A] (ya: Yoneda [F, A]) = lowerYoneda [F, A] (ya)
  def yoLo [F [_], A] (ya: Yoneda [F, A]) = lowerYoneda [F, A] (ya)

  object instances {
    implicit def persiansStdFunctorForYoneda [F [_]] = new Functor [Yoneda [F, ?]] {
      def map [A, B] (fa: Yoneda [F, A])(f: A => B) =
        new Yoneda [F, B] {
          def roYo = new (λ [C => (B => C)] ~> F) {
            def apply [T] (k: B => T) = fa roYo (k compose f)
          }
        }
    }

    implicit def persiansStdApplyForYoneda [F [_] : Apply] = new Apply [Yoneda [F, ?]] {
      import syntax.functorSyntaxForYoneda
      def map [A, B] (ya: Yoneda [F, A])(f: A => B) = persiansStdFunctorForYoneda .map (ya) (f)
      def ap [A, B] (yf: Yoneda [F, A => B])(ya: Yoneda [F, A]): Yoneda [F, B] =
        new Yoneda [F, B] {
          def roYo = new (λ [C => (B => C)] ~> F) {
            def apply [T] (k: B => T) =
              (Yoneda loYo (yf map (k compose _))) ap (
                Yoneda loYo  ya)
          }
        }
    }

    implicit def persiansStdApplicativeForYoneda [F [_] : Applicative] = new Applicative [Yoneda [F, ?]] {
      import syntax.applySyntaxForYoneda
      def ap [A, B] (yf: Yoneda [F, A => B])(ya: Yoneda [F, A]): Yoneda [F, B] = ya ap yf
      def pure [A] (a: A) = Yoneda upYo (implicitly [Applicative [F]] pure a)
    }
  }

  object syntax {
    implicit class functorSyntaxForYoneda [F [_], A] (ya: Yoneda [F, A]) {
      import instances.persiansStdFunctorForYoneda
      def map [B] (f: A => B) = persiansStdFunctorForYoneda.map [A, B] (ya)(f)
    }

    implicit class applySyntaxForYoneda [F [_] : Apply, A] (ya: Yoneda [F, A]) {
      import instances.persiansStdApplyForYoneda
      def map [B] (f: A => B) = persiansStdApplyForYoneda [F] .map [A, B] (ya)(f)
      def ap [B] (yf: Yoneda[F, A => B]) = persiansStdApplyForYoneda [F] .ap [A, B] (yf)(ya)
    }

    implicit class applicativeSyntaxForYoneda [F [_] : Applicative, A] (ya: Yoneda [F, A]) {
      import instances.persiansStdApplicativeForYoneda
      def ap [B] (yf: Yoneda [F, A => B]) = persiansStdApplicativeForYoneda [F] .ap [A, B] (yf)(ya)
    }
  }
}

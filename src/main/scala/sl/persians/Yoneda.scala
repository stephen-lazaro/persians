package sl.persians

import cats.{Applicative, Apply, Functor, Monad}
import cats.syntax.apply._
import cats.syntax.flatMap._
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
  * This insight shamelessly stolen from the ekmett's docs.
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
  /**
    * liftYoneda and lowerYoneda are the critical morphisms exhibiting Yoneda's lemma
    * Yoneda's lemma says:
    *   liftYoneda compose lowerYoneda = Y [F, _] ~> Y [F, _] identity nat transform
    *   lowerYoneda compose liftYoneda = F ~> F identity nat transform
    * where F : Functor, i.e. F is a functor.
    */
  def liftYoneda [F [_]: Functor, A] (fa: F[A]): Yoneda [F, A] = new Yoneda [F, A] {
    def roYo = new (λ [B => (A => B)] ~> F) {
      def apply [T] (f: A => T) = implicitly [Functor [F]] .map (fa)(f)
    }
  }
  // Apparently Scala wont' let these can't be eta reduced, though I am not sure why
  def upYo [F [_] : Functor, A] (fa: F [A]) = liftYoneda [F, A] (fa)
  def yUp  [F [_] : Functor, A] (fa: F [A]) = liftYoneda [F, A] (fa)
  def liYo [F [_] : Functor, A] (fa: F [A]) = liftYoneda [F, A] (fa)

  def lowerYoneda [F [_], A] (ya: Yoneda [F, A]): F [A] =
    ya roYo identity [A] _
  // Apparently Scala wont' let these can't be eta reduced, though I am not sure why
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

    implicit def persiansStdMonadForYoneda [F [_] : Monad] = new Monad [Yoneda [F, ?]] {
      def pure [A] (a: A) = persiansStdApplicativeForYoneda pure a

      def flatMap [A, B] (ya: Yoneda [F, A])(yf: A => Yoneda [F, B]) =
        Yoneda upYo ((Yoneda loYo ya) flatMap ((Yoneda loYo[F, B] _) compose yf))

      // Assumes F has a stacksafe Monad implemented.
      def tailRecM [A, B] (a: A)(f: A => Yoneda[F, Either [A, B]]): Yoneda[F, B] =
        Yoneda.upYo (implicitly [Monad[F]] .tailRecM (a)(f andThen Yoneda.loYo))
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
      def pure (a: A) = persiansStdApplicativeForYoneda [F] pure a
      def map [B] (f: A => B) = persiansStdApplicativeForYoneda [F] .map (ya)(f)
    }

    implicit class monadSyntaxForYoneda [F [_] : Monad, A] (ya: Yoneda [F, A]) {
      import instances.persiansStdMonadForYoneda
      def pure (a: A) = persiansStdMonadForYoneda [F] pure a
      def map [B] (f: A => B) = persiansStdMonadForYoneda [F] .map (ya)(f)
      def flatMap [B] (f: A => Yoneda [F, B]) = persiansStdMonadForYoneda [F] .flatMap [A, B] (ya)(f)
    }
  }

  def maxF  [F [_] : Functor, A] (ya: Yoneda [F, A])(yb: Yoneda [F, A])(
    implicit o: Ordering [F [A]]): Yoneda [F, A] =
      Yoneda upYo (o max (Yoneda loYo ya, Yoneda loYo yb))

  def minF [F [_] : Functor, A] (ya: Yoneda [F, A])(yb: Yoneda [F, A])(
    implicit o: Ordering [F [A]]): Yoneda [F, A] =
      Yoneda upYo (o min (Yoneda loYo ya, Yoneda loYo yb))

  def maxM  [F [_] : Monad, A] (ya: Yoneda [F, A])(yb: Yoneda [F, A])(
    implicit o: Ordering [F [A]]): Yoneda [F, A] = maxF [F, A] (ya)(yb)

  def minM  [F [_] : Monad, A] (ya: Yoneda [F, A])(yb: Yoneda [F, A])(
    implicit o: Ordering [F [A]]): Yoneda [F, A] = minF [F, A] (ya)(yb)

  def maxMlifted [M [_] : Monad, A : Ordering] (ya: Yoneda [M, A])(yb: Yoneda [M, A]) = {
    import syntax.monadSyntaxForYoneda
    ya flatMap [A] (a => yb map [A] (b => implicitly [Ordering [A]] max (a, b)))
  }

  def minMlifted [M [_] : Monad, A : Ordering] (ya: Yoneda [M, A])(yb: Yoneda [M, A]) = {
    import syntax.monadSyntaxForYoneda
    ya flatMap [A] (a => yb map [A] (b => implicitly [Ordering [A]] min (a, b)))
  }

}

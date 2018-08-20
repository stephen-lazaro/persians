package sl.persians.kan

import cats.{Functor, Id, Monad, ~>}
import cats.data.Kleisli
import cats.effect.{IO, LiftIO}
import cats.free.Yoneda
import sl.persians.{Adjunction, Codensity}

import scala.concurrent.Future

trait Ran [G [_], H [_], A] {
  def run[B](given: A => G [B]): H [B]
}
object Ran {
  def toRan[F[_]: Functor, G[_], H[_], B](trans: λ[A => F [G [A]]] ~> H)(fb: F[B]): Ran[G, H, B] =
    new Ran [G, H, B] {
      def run [C] (given: B => G[C]): H[C] = trans.apply[C](Functor[F].map(fb)(given))
    }

  def fromRan[F[_], G[_], H[_], B](trans: F ~> Ran[G, H, ?])(fgb: F[G[B]]): H[B] =
    trans.apply[G[B]](fgb).run(identity[G[B]])

  def undo [G[_], H[_], A](ran: Ran[G, H, G[A]]): H[A] = ran.run[A](identity[G[A]])

  def fromLiftIO[G[_]: LiftIO, A](a: A): Ran[IO, G, A] =
    new Ran[IO, G, A] {
      def run[B](given: A => IO[B]): G[B] = LiftIO[G].liftIO(given(a))
    }

  def unsafeIOAsFuture[A](a: A): Ran[IO, Future, A] =
    new Ran[IO, Future, A] {
      def run[B](given: A => IO[B]): Future[B] = given(a).unsafeToFuture
    }

  def asOption[E, A](a: A): Ran[Either[E, ?], Option, A] =
    new Ran[Either[E, ?], Option, A] {
      def run[B](given: A => Either[E, B]): Option[B] = given(a).toOption
    }

  def toYoneda[F[_], A](ran: Ran[Id, F, A]): Yoneda[F, A] =
    new Yoneda[F, A] {
      def apply[B](f: A => B): F[B] = ran.run(f)
    }

  def fromYoneda[F[_], A](yoneda: Yoneda[F, A]): Ran[Id, F, A] =
    new Ran[Id, F, A] {
      def run[B](f: A => B): F[B] = yoneda(f)
    }

  def fromAdjunction[F[_], G[_], A](fa: F[A])(
    implicit
    adjunction: Adjunction[F, G]
  ): Ran[G, Id, A] = new Ran[G, Id, A] {
    def run[B](given: A => G[B]): B = Adjunction[F, G].rightAdjoint(given)(fa)
  }

  def toAdjunction[F[_], G[_], A](ran: Ran[G, Id, A])(
    implicit
    adjunction: Adjunction[F, G]
  ): F[A] = ran.run((a: A) => Adjunction[F, G].unit(a))

  def withKleisli[F[_]: Monad, G[_], A, BB](a: A)(trans: F ~> G): Kleisli[F, A, BB] => G[BB] =
    kleisli => Ran.transform[F, F, G, A](trans)(Ran.trivial[F, A](a)).run(kleisli.run)

  def trivial[F[_], A](a: A): Ran[F, F, A] =
    new Ran[F, F, A] {
      def run[B](given: A => F[B]): F[B] = given(a)
    }

  def transform[F[_], G[_], H[_], A](f: F ~> H)(ran: Ran[G, F, A]): Ran[G, H, A] =
    new Ran[G, H, A] {
      def run[B](h: A => G[B]): H[B] = f.apply[B](ran.run(h))
    }

  def toCodensity[G[_], A](ran: Ran[G, G, A]): Codensity[G, A] =
    new Codensity[G, A] {
      def run[B](given: A => G[B]): G[B] = ran.run(given)
    }

  implicit def functorForRan[G[_], H[_]]: Functor[Ran[G, H, ?]] =
    new Functor[Ran[G, H, ?]] {
      def map[A, B](fa: Ran[G, H, A])(f: A => B): Ran[G, H, B] =
        new Ran[G, H, B] {
          def run [C] (given: B => G[C]): H[C] =
           fa.run[C](given compose f)
        }
    }
}

package sl.persians.kan

import cats.{~>, Functor}
import cats.effect.{IO, LiftIO}

import sl.persians.Codensity

trait Ran [G [_], H [_], A] {
  def run[B](given: A => G [B]): H [B]
}
object Ran {
  def toRan[F[_]: Functor, G[_], H[_], B](trans: λ[A => F [G [A]]] ~> H)(fb: F[B]): Ran[G, H, B] =
    new Ran [G, H, B] {
      def run [C] (given: B => G[C]): H[C] = trans.apply[C](Functor[F].map(fb)(given))
    }

  def fromRan [F[_], G[_], H[_], B](trans: F ~> Ran[G, H, ?])(fgb: F[G[B]]): H[B] =
    trans.apply[G[B]](fgb).run(identity[G[B]])

  def undo [G[_], H[_], A](ran: Ran[G, H, G[A]]): H[A] = ran.run[A](identity[G[A]])

  def fromLiftIO[G[_]: LiftIO, A](a: A): Ran[IO, G, A] =
    new Ran[IO, G, A] {
      def run[B](given: A => IO[B]): G[B] = LiftIO[G].liftIO(given(a))
  }

  def asOption[E, A](a: A): Ran[Either[E, ?], Option, A] =
    new Ran[Either[E, ?], Option, A] {
      def run[B](given: A => Either[E, B]): Option[B] = given(a).toOption
    }

  def trivial[F[_], A](a: A): Ran[F, F, A] =
    new Ran[F, F, A] {
      def run[B](given: A => F[B]): F[B] = given(a)
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

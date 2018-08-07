package sl.persians

import cats.{Functor, Id, Representable}
import cats.arrow.{ArrowChoice, Profunctor}
import cats.data.{EitherK, Tuple2K}
import cats.instances.tuple.catsStdInstancesForTuple2
import cats.instances.function.catsStdMonadForFunction1
import cats.instances.function.catsStdRepresentableForFunction1
import cats.instances.function.catsStdInstancesForFunction1
import cats.syntax.arrowChoice.toArrowChoiceOps

trait Adjunction[F[_], U[_]] {
  val F: Functor[F]
  val U: Representable[U]
  type Repr = U.Representation

  def unit[A](a: A): U[F[A]]
  def counit[A](a: F[U[A]]): A
  def leftAdjoint[A, B](f: F[A] => B): A => U[B] = (a: A) => U.F.map(unit(a))(f)
  def rightAdjoint[A, B](f: A => U[B]): F[A] => B = (fa: F[A]) => counit(F.map(fa)(f))
}
object Adjunction {
  def apply[F[_], U[_]](
    implicit
    adj: Adjunction[F, U]
  ): Adjunction[F, U] = implicitly[Adjunction[F, U]]

  // Scala is a cartesian category
  implicit def cartesianCategory[A, ?]: Adjunction[(A, ?), A => ?] =
    new Adjunction[(A, ?), A => ?] {
      val F = Functor[(A, ?)]
      val U = Representable[A => ?]

      def unit[B](b: B): A => (A, B) = (a: A) => (a, b)
      def counit[B](b: (A, A => B)): B = b match {
        case (seed, f) => f(seed)
      }
    }

  def adjuncted[F[_], U[_], P[_, _]: Profunctor, G[_]: Functor, A, B, C, D](
    p: P[A => U[B], G[C => U[D]]]
    )(
    implicit
    _adjunction: Adjunction[F, U]
  ): P[F[A] => B, G[F[C] => D]] = Profunctor[P].dimap(p)(
      Adjunction[F, U].leftAdjoint[A, B])(
      Functor[G].lift(Adjunction[F, U].rightAdjoint[C, D])
    )

  def tabulateAdjunction[F[_], U[_], B](fu: F[Unit] => B)(
    implicit
    _adj: Adjunction[F, U]
  ): U[B] =
    Adjunction[F, U].leftAdjoint(fu).apply(())

  def indexAdjunction[F[_], U[_], A, B](ub: U[B])(fa: F[A])(
    implicit
    _adj: Adjunction[F, U]
  ): B = Adjunction[F, U].rightAdjoint[A, B](Function.const(ub)).apply(fa)

  def cozipLeft[F[_], U[_], A, B](implicit
    adjunction: Adjunction[F, U]
  ): F[Either[A, B]] => Either[F[A], F[B]] =
    Adjunction[F, U].rightAdjoint(
       ArrowChoice[Function1].choice[A, B, U[Either[F[A], F[B]]]](
        Adjunction[F, U].leftAdjoint(Left.apply[F[A], F[B]]),
        Adjunction[F, U].leftAdjoint(Right.apply[F[A], F[B]])
      )
    )

  implicit def adjunctionForId: Adjunction[Id, Id] =
    new Adjunction[Id, Id] {
      val F = Functor[Id]
      val U = Representable[Id]
      def unit[A](a: A): A = a
      def counit[A](fa: A): A = fa
    }


  implicit def representableForTuple2K[F[_]: Functor, G[_]: Functor]: Representable[Tuple2K[F, G, ?]] = ???
  implicit def adjunctionForTuple2KAndEither2K[F[_]: Functor, G[_]: Functor, H[_]: Functor, I[_]: Functor](
    implicit
    _adjA: Adjunction[F, H],
    _adjB: Adjunction[G, I]
  ): Adjunction[EitherK[F, H, ?], Tuple2K[G, I, ?]] =
    new Adjunction[EitherK[F, H, ?], Tuple2K[G, I, ?]] {
      val F = Functor[EitherK[F, H, ?]]
      val U = Representable[Tuple2K[G, I, ?]]
      def unit[B](b: B):  Tuple2K[G, I, EitherK[F, H, B]] = ???
      def counit[B](fgb: EitherK[F, H, Tuple2K[G, I, B]]): B = ???
    }
}

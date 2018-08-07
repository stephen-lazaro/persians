package sl.persians

import cats.{Functor, Id, Representable}
import cats.arrow.Profunctor
import cats.instances.tuple.catsStdInstancesForTuple2
import cats.instances.function.catsStdMonadForFunction1
import cats.instances.function.catsStdRepresentableForFunction1
import cats.syntax.profunctor.toProfunctorOps

trait Adjunction[F[_], U[_]] {
  val F: Functor[F]
  val U: Representable[U]
  type Repr = U.Representation

  def unit[A](a: A): U[F[A]]
  def counit[A](a: F[U[A]]): A
  // WTF?
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


  implicit def adjunctionForId: Adjunction[Id, Id] =
    new Adjunction[Id, Id] {
      val F = Functor[Id]
      val U = Representable[Id]
      def unit[A](a: A): A = a
      def counit[A](fa: A): A = fa
    }

}

package sl.persians

import cats.{Comonad, Functor, Id, Monad, Representable}
import cats.arrow.{ArrowChoice, Profunctor}
import cats.data.{EitherK, Nested, Tuple2K}
import cats.free.{Cofree, Free}
import cats.instances.tuple.catsStdInstancesForTuple2
import cats.instances.function.catsStdMonadForFunction1
import cats.instances.function.catsStdRepresentableForFunction1
import cats.instances.function.catsStdInstancesForFunction1
import cats.syntax.arrow.toArrowOps
import cats.syntax.functor.toFunctorOps

/**
  * Based on:
  * http://hackage.haskell.org/package/adjunctions
  */
trait Adjunction[F[_], U[_]] {
  val F: Functor[F]
  val U: Representable[U]
  def UF: Functor[U] = U.F
  type Repr = U.Representation

  def unit[A](a: A): U[F[A]]
  def counit[A](a: F[U[A]]): A
  def leftAdjoint[A, B](f: F[A] => B): A => U[B] = (a: A) => UF.map(unit(a))(f)
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

  // Exchange either side of a profunctor by applying the adjunction
  def adjoined[F[_], U[_], P[_, _]: Profunctor, G[_]: Functor, A, B, C, D](
    p: P[A => U[B], G[C => U[D]]])(
    implicit
    _adjunction: Adjunction[F, U]
  ): P[F[A] => B, G[F[C] => D]] =
    Profunctor[P].dimap(p)(
      Adjunction[F, U].leftAdjoint[A, B])(
      Functor[G].lift(Adjunction[F, U].rightAdjoint[C, D]))

  def tabulateAdjunction[F[_], U[_], B](fu: F[Unit] => B)(
    implicit
    _adj: Adjunction[F, U]
  ): U[B] =
    Adjunction[F, U].leftAdjoint(fu).apply(())

  def indexAdjunction[F[_], U[_], A, B](ub: U[B])(fa: F[A])(
    implicit
    _adj: Adjunction[F, U]
  ): B = Adjunction[F, U].rightAdjoint[A, B](Function.const(ub)).apply(fa)

  def cozipLeft[F[_], U[_], A, B](feither: F[Either[A, B]])(
    implicit
    adjunction: Adjunction[F, U]
  ): Either[F[A], F[B]] =
    Adjunction[F, U].rightAdjoint(
       ArrowChoice[Function1].choice[A, B, U[Either[F[A], F[B]]]](
        Adjunction[F, U].leftAdjoint(Left.apply[F[A], F[B]]),
        Adjunction[F, U].leftAdjoint(Right.apply[F[A], F[B]])
      )
    )(feither)

  def zipRight[F[_], U[_], A, B](uab: (U[A], U[B]))(
    implicit
    adjunction: Adjunction[F, U]
  ): U[(A, B)] =
    Adjunction[F, U].leftAdjoint(
      Adjunction[F, U].rightAdjoint[(U[A], U[B]), A](_._1) &&&
      Adjunction[F, U].rightAdjoint[(U[A], U[B]), B](_._2)
    )(uab)

  def wrapWithAdjunction[F[_], U[_], A, B, C](f: A => B => C)(
    implicit
    adj: Adjunction[F, U]
  ): U[A] => F[B] => C = {
    val leftAdjoint = Adjunction[F, U].F
    val rightAdjoint = Adjunction[F, U].UF
    (ua: U[A]) => (fb: F[B]) =>
      Adjunction[F, U].counit(
        leftAdjoint.map(fb)(b =>
          rightAdjoint.map(
            rightAdjoint.map(ua)(f))(
            _.apply(b))))
  }

  def extractLeft[F[_], U[_], A](fa: F[A])(
    implicit
    adj: Adjunction[F, U]
  ): A = splitLeft(fa)._1

  def duplicateLeft[F[_], U[_], A](fa: F[A])(
    implicit
    adj: Adjunction[F, U]
  ): F[F[A]] = Adjunction[F, U].F.as(splitLeft(fa)._2, fa)

  def splitLeft[F[_], U[_], A](fa: F[A])(
    implicit
    adj: Adjunction[F, U]
  ): (A, F[Unit]) =
    Adjunction[F, U].rightAdjoint((a: A) =>
      Adjunction[F, U].leftAdjoint((inner: F[A]) =>
        (a, Adjunction[F, U].F.void(inner))
      )(a)
    )(fa)

  implicit def adjunctionForId: Adjunction[Id, Id] =
    new Adjunction[Id, Id] {
      val F = Functor[Id]
      val U = Representable[Id]

      def unit[A](a: A): A = a

      def counit[A](fa: A): A = fa
    }

  implicit def representableCofree[F[_]](
    implicit
    representable: Representable[F]
  ): Representable[Cofree[F, ?]] = new Representable[Cofree[F, ?]] {
    implicit val F_ = Representable[F].F
    def F: Functor[Cofree[F, ?]] = Comonad[Cofree[F, ?]]

    type Repr = representable.Representation
    type Representation = Stream[Repr]

    def index[A](f: Cofree[F, A]): Representation => A =
      Function.const(Comonad[Cofree[F, ?]].extract(f))

    def tabulate[A](f: Representation => A): Cofree[F, A] =
      Cofree.unfold[F, A](f(Stream.empty[Repr]))(
        _ => representable.tabulate((x: Repr) => f(Stream(x))))
  }

  implicit def adjunctionForFreeCofree[F[_], U[_]](
    implicit
    adj: Adjunction[F, U]
  ): Adjunction[Free[F, ?], Cofree[U, ?]] =
    new Adjunction[Free[F, ?], Cofree[U, ?]] {
      val F = Functor[Free[F, ?]]
      implicit val R: Representable[U] = Adjunction[F, U].U
      val U = Representable[Cofree[U, ?]] // Gonna need to supply this one

      implicit val F_ = Adjunction[F, U].F
      implicit val U_ = Adjunction[F, U].UF

      def unit[A](a: A): Cofree[U, Free[F, A]] =
        Functor[Cofree[U, ?]].as(
          Cofree.unfold(a)(a => Adjunction[F, U].U.tabulate(Function.const(a))),
          Free.pure[F, A](a)
        )

      def counit[A](a: Free[F, Cofree[U, A]]): A =
        Comonad[Cofree[U, ?]].extract[A](
          a.go(extractLeft[F, U, Free[F, Cofree[U, A]]])
        )
    }

  implicit def comonadFromAdjunction[F[_], G[_]](
    implicit
    adj: Adjunction[F, G]
  ): Comonad[λ[α => F[G[α]]]] =
    new Comonad[λ[α => F[G[α]]]] {
      implicit val F_ = adj.F
      implicit val G_ = adj.UF

      def extract[A](x: F[G[A]]): A = Adjunction[F, G].counit(x)

      def coflatMap[A, B](fa: F[G[A]])(f: (F[G[A]]) => B): F[G[B]] =
        Adjunction[F, G].F.map(fa)(Adjunction[F, G].leftAdjoint(f))

      def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] =
        Nested[F, G, A](fa).map(f).value
    }

  implicit def monadFromAdjunction[F[_], G[_]](
    implicit
    adj: Adjunction[F, G]
  ): Monad[λ[α => G[F[α]]]] =
    new Monad[λ[α => G[F[α]]]] {
      def pure[A](x: A): G[F[A]] = Adjunction[F, G].unit(x)

      def flatMap[A, B](fa: G[F[A]])(f: (A) => G[F[B]]): G[F[B]] =
        Adjunction[F, G].UF.map(fa)(Adjunction[F, G].rightAdjoint(f))

      // FIXME: Not stack safe by default. Possible?
      def tailRecM[A, B](a: A)(f: (A) => G[F[Either[A, B]]]): G[F[B]] = {
        def go(e: Either[A, B]): G[F[B]] = e match {
          case Right(b) => pure(b)
          case Left(a_) => flatMap(f(a_))(go)
        }

        flatMap(f(a))(go)
      }
    }

  implicit def representableForTuple2K[F[_], G[_]](
    implicit
    reprF: Representable[F],
    reprG: Representable[G]
  ): Representable[Tuple2K[F, G, ?]] =
    new Representable[Tuple2K[F, G, ?]] {
      implicit val F_ = Representable[F].F
      implicit val G_ = Representable[G].F
      def F: Functor[Tuple2K[F, G, ?]] = Functor[Tuple2K[F, G, ?]]

      type LeftRepr = reprF.Representation
      type RightRepr = reprG.Representation
      type Representation = Either[LeftRepr, RightRepr]

      def index[A](f: Tuple2K[F, G, A]): Representation => A =
        ArrowChoice[Function1].choice[LeftRepr, RightRepr, A](
          reprF.index(f.first),
          reprG.index(f.second)
        )

      def tabulate[A](f: Representation => A): Tuple2K[F, G, A] =
        Tuple2K(
          reprF.tabulate((r: LeftRepr) => f(Left(r))),
          reprG.tabulate((r: RightRepr) => f(Right(r)))
        )
    }

  implicit def adjunctionForTuple2KAndEither2K[F[_]: Functor, G[_]: Functor, H[_]: Functor, I[_]: Functor](
    implicit
    _adjA: Adjunction[F, H],
    _adjB: Adjunction[G, I]
  ): Adjunction[EitherK[F, G, ?], Tuple2K[H, I, ?]] =
    new Adjunction[EitherK[F, G, ?], Tuple2K[H, I, ?]] {
      implicit val RH: Representable[H] = _adjA.U
      implicit val RI: Representable[I] = _adjB.U
      val F = Functor[EitherK[F, G, ?]]
      val U = Representable[Tuple2K[H, I, ?]]
      def unit[A] (a: A): Tuple2K[H, I, EitherK[F, G, A]] = ???
      def counit[A] (a: EitherK[F, G, Tuple2K[H, I, A]]): A = ???
    }
}

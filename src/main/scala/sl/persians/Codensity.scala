package sl.persians

import cats.{~>, Alternative, Applicative, Apply, Functor, Monad}
import cats.effect.{Bracket, IO, LiftIO, Resource}
import cats.free.Free

import sl.persians.kan.Ran

trait Codensity[K[_], A] {
  def run[B](f: A => K[B]): K[B]
}
object Codensity {
  def toRan[G[_], A](codense: Codensity[G, A]): Ran[G, G, A] =
    new Ran[G, G, A] {
      def run[B](given: A => G[B]) = codense.run(given)
    }

  def wrapWith[M[_]](trans: M ~> M): Codensity[M, Unit] =
    new Codensity[M, Unit] {
      def run[B](f: Unit => M[B]): M[B] =
        (f andThen trans.apply[B]).apply(())
    }

  def appendFinally[M[_]](trans: M ~> M): Codensity[M, Unit] =
    wrapWith[M](trans)

  def toResource[M[_], A, B, E](codensity: Codensity[M, A])(
    implicit
    _bracket: Bracket[M, E]
  ): Resource[M, A] =
    // Treat the codensity as _supplying_ a resource,
    // to be released trivially, as codensity
    // isn't tracking finalization.
    Resource.make(
      codensity.run(Bracket[M, E].pure))(
      Function.const(Bracket[M, E].unit))

  def fromResource[A, M[_]](ma: Resource[M, A])(
    implicit
    _bracket: Bracket[M, Throwable]
    ): Codensity[M, A] =
    new Codensity[M, A] {
      def run[B](f: A => M[B]): M[B] = ma.use(f)
    }

  def lowerCodensity[F[_]: Applicative, A](codensity: Codensity[F, A]): F[A] =
    codensity.run[A](Applicative[F].pure)

  // Todo: Monad => MonadFree HMMMMMMM
  def improve[F[_]: Functor, M[_]: MonadFree: Monad, A](toImprove: M[A]): M[A] =
    lowerCodensity(new Codensity[M, A]{
      // Is this the correct implementation?
      def run[B](g: A => M[B]) = Monad[M].flatMap(toImprove)(g)
    })

  trait MonadFree[M[_]] {}
  implicit def monadFreeForFree[F[_]: Functor] = new MonadFree[Free[F, ?]] {}
  implicit val monadFreeForIO = new MonadFree[IO] {}

  trait FunctorForCodensity[F[_]] extends Functor[Codensity[F, ?]] {
    def map[A, BB](codensity: Codensity[F, A])(f: A => BB): Codensity[F, BB] =
      new Codensity[F, BB] {
        def run[B](g: BB => F[B]): F[B] =
          codensity.run(f andThen g)
      }
  }

  trait ApplyForCodensity[F[_]] extends FunctorForCodensity[F] with Apply[Codensity[F, ?]] {
    def ap[A, BB](ff: Codensity[F, A => BB])(fa: Codensity[F, A]): Codensity[F, BB] =
      new Codensity[F, BB] {
        def run[B](f: BB => F[B]): F[B] =
          fa.run((x: A) => ff.run((g: A => BB) => f(g(x))))
      }
  }

  trait ApplicativeForCodensity[F[_]] extends ApplyForCodensity[F]  with Applicative[Codensity[F, ?]] {
    def pure[A](x: A): Codensity[F, A] =
      new Codensity[F, A] {
        def run[B](g: A => F[B]): F[B] = g(x)
      }
  }

  trait MonadForCodensity[F[_]] extends ApplicativeForCodensity[F] with Monad[Codensity[F, ?]] {
    def flatMap[A, BB](fa: Codensity[F, A])(ff: A => Codensity[F, BB]): Codensity[F, BB] =
      new Codensity[F, BB] {
        def run[B](f: BB => F[B]): F[B] = fa.run((x: A) => ff(x).run(f))
      }

    def tailRecM[A, BB](a: A)(f: A => Codensity[F, Either[A, BB]]): Codensity[F, BB] =
      new Codensity[F, BB] {
        def run[B](h: BB => F[B]): F[B] =
          f(a).run((e: Either[A, BB]) => e match {
            case Right(b) => h(b)
            case Left(a_) => tailRecM(a)(f).run(h)
          })
      }
  }

  trait LiftIOForCodensity[F[_]] extends MonadForCodensity[F] with LiftIO[Codensity[F, ?]] {
    val L: LiftIO[F]
    val F: Monad[F]
    def liftIO[A](ioa: IO[A]): Codensity[F, A] =
      new Codensity[F, A] {
        def run[B](f: A => F[B]) = F.flatMap(L.liftIO(ioa))(f)
      }
  }

  trait AlternativeForCodensity[F[_]] extends ApplicativeForCodensity[F] with Alternative[Codensity[F, ?]] {
    val F: Alternative[F]

    def empty[A]: Codensity[F, A] = new Codensity[F, A] {
      def run[B](f: A => F[B]): F[B] = F.empty[B]
    }
    def combineK[A](x: Codensity[F, A], y: Codensity[F, A]): Codensity[F, A] =
      new Codensity[F, A] {
        def run[B](f: A => F[B]) = F.combineK[B](x.run[B](f), y.run[B](f))
      }
  }
}
trait Implicits extends Low {
  import Codensity._
  implicit def alternativeForCodensity[F[_]: Alternative]: Alternative[Codensity[F, ?]] =
    new AlternativeForCodensity[F] {
      val F = Alternative[F]
    }

  implicit def liftIoForCodensity[F[_]: Monad: LiftIO]: LiftIO[Codensity[F, ?]] =
    new LiftIOForCodensity[F] {
      val L = LiftIO[F]
      val F = Monad[F]
    }
}
trait Low {
  import Codensity._
  implicit def monadForCodensity[F[_]: Applicative]: Monad[Codensity[F, ?]] =
    new MonadForCodensity[F] {}
}

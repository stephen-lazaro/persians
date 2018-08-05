package sl
package persians

import cats.{~>, Comonad, Functor, Id, Monad}
import cats.data.Const

trait CoT[W[_], M[_], A] {
  def run[B](given: W[A => M[B]]): M[B]
}
object CoT {
  def liftComonadToCoT[W[_]: Comonad, M[_], S](trans: W ~> Const[S, ?]): CoT[W, M, S] =
    new CoT[W, M, S] {
      def run[B](given: W[S => M[B]]): M[B] =
        Comonad[W].extract(given)(trans.apply(given).getConst)
    }

  // Weird that we "throw away" the wa value, only using its context.
  def lowerFromCoT[W[_]: Functor, M[_]: Monad, S, A](cot: CoT[W, M, S])(wa: W[A]): M[S] =
    cot.run(Functor[W].map(wa)(Function.const(Monad[M].pure)))

  def lowerFromCo[W[_]: Functor, S, A](co: Co[W, S])(wa: W[A]): S =
    lowerFromCoT[W, Id, S, A](co)(wa)

  def liftToCoTUnit[W[_], M[_], S](trans: W ~> Id): CoT[W, M, Unit] =
    new CoT[W, M, Unit] {
      def run[B](given: W[Unit => M[B]]): M[B] =
        trans.apply(given).apply(())
    }

  def lowerCotWithUnit[W[_]: Functor, M[_]: Monad, A](cot: CoT[W, M, Unit])(wa: W[A]): M[A] =
    cot.run(Functor[W].map(wa)(a => Unit => Monad[M].pure(a)))

  def lowerCoUnit[W[_]: Functor, A](co: Co[W, A])(wa: W[A]): A =
    co.run(Functor[W].map(wa)(Function.const[A, A]))

  def liftToCoTFromMonadAndComonad[W[_]: Comonad, M[_]: Monad, S](trans: W ~> Const[M[S], ?]): CoT[W, M, S] =
    new CoT[W, M, S] {
      def run[B](given: W[S => M[B]]): M[B] =
        Monad[M].flatMap(trans.apply(given).getConst)(Comonad[W].extract(given))
    }

  def liftToCoTFromMonad[W[_], M[_]: Monad](trans: W ~> M): CoT[W, M, Unit] =
    new CoT[W, M, Unit] {
      def run[B](given: W[Unit => M[B]]): M[B] =
        Monad[M].flatMap(trans.apply(given))(_.apply(()))
    }

  implicit def monadFromComonad[W[_]: Comonad]: Monad[Co[W, ?]] =
    new Monad[Co[W, ?]] {
      def flatMap[A, BB](fa: Co[W, A])(ff: A => Co[W, BB]): Co[W, BB] =
        new CoT[W, Id, BB] {
          def run[B](given: W[BB => B]): B =
            fa.run(Comonad[W].coflatMap(given)(
              (wbb: W[BB => B]) => (a: A) => ff(a).run(given)))
        }

      def pure[A](a: A): Co[W, A] = new CoT[W, Id, A] {
        def run[B](given: W[A => B]): B =
          Comonad[W].extract(given)(a)
      }

      def tailRecM[A, BB](a: A)(f: A => CoT[W, Id, Either[A, BB]]): Co[W, BB] =
        new CoT[W, Id, BB] {
          def run[B](given: W[BB => B]): B =
            f(a).run(
              Comonad[W].coflatMap(given)((mf: W[BB => B]) =>
                (e: Either[A, BB]) => e match {
                  case Right(bb) => Comonad[W].extract(mf)(bb)
                  case Left(a_) => tailRecM(a_)(f).run(mf)
                }))
        }
    }
}

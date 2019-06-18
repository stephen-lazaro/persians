package sl.persians

import cats.arrow.FunctionK
import cats.data.Nested
import cats.{Distributive, Functor, Id, Representable, ~>}
import sl.persians.HFunctor.{Logarithm, Tabulation}

trait HFunctor[W[?[_], _]] {
  def hmap[F[_], G[_]](woo: F ~> G): W[F, ?] ~> W[G, ?]
}
object HFunctor {
  def apply[W[?[_], _]: HFunctor]: HFunctor[W] = implicitly

  type Tabulation[F[_], A] = (F ~> Id) => A
  implicit val higherFunctorForTabluations: HFunctor[Tabulation] = new HFunctor[Tabulation] {
    override def hmap[F[_], G[_]](woo: F ~> G): Tabulation[F, ?] ~> Tabulation[G, ?] =
      new (Tabulation[F, ?] ~> Tabulation[G, ?]) {
        override def apply[A](fa: Tabulation[F, A]): Tabulation[G, A] =
          (h: G ~> Id) => fa(woo andThen h)
      }
  }

  case class BindArg[F[_], A, B](fa: F[A], ff: A => F[B])
  object BindArg {
    implicit def higherFunctorForBindArg[B]: HFunctor[BindArg[?[_], ?, B]] = new HFunctor[BindArg[?[_], ?, B]] {
      override def hmap[F[_], G[_]](woo: F ~> G): BindArg[F, ?, B] ~> BindArg[G, ?, B] =
        new (BindArg[F, ?, B] ~> BindArg[G, ?, B]) {
          override def apply[A](fa: BindArg[F, A, B]): BindArg[G, A, B] = fa match {
            case BindArg(value, bound) =>
              BindArg(woo.apply(value), bound andThen woo.apply[B])
          }
        }
    }
  }

  type Pair[F[_], A, B] =  (F[A], F[B])
  implicit def higherFunctorForPairLeft[A]: HFunctor[Pair[?[_], A, ?]] = new HFunctor[Pair[?[_], A, ?]] {
    override def hmap[F[_], G[_]](woo: F ~> G): Pair[F, A, ?] ~> Pair[G, A, ?] =
      new (Pair[F, A, ?] ~> Pair[G, A, ?]) {
        override def apply[B](fa: Pair[F, A, B]): Pair[G, A, B] = fa match {
          case (fa, fb) => (woo.apply(fa), woo(fb))
        }
      }
  }

  type Logarithm[F[_]] = F ~> Id
}

trait Naperian[F[_]] {
  val F: Distributive[F]

  def hdistribute[W[?[_], _]: HFunctor]: W[F, ?] ~> Nested[F, W[Id, ?], ?]

  def ncotraverse[W[?[_], _]: HFunctor, A, B](f: W[Id, A] => B)(wf: W[F, A]): F[B] =
    F.lift(f).apply(hdistribute[W].apply[A](wf).value)

  def ncollect[W[?[_], _]: HFunctor, G[_]](f: G ~> F): W[G, ?] ~> Nested[F, W[Id, ?], ?] =
    new (W[G, ?] ~> Nested[F, W[Id, ?], ?]) {
      override def apply[A](fa: W[G, A]): Nested[F, W[Id, ?], A] =
        hdistribute.apply(HFunctor[W].hmap(f)(fa))
    }

  def ntwiddle[W[?[_], _]: HFunctor, G[_], A, B](f: W[Id, A] => B)(ff: G ~> F)(wg: W[G, A]): F[B] =
    F.map(hdistribute.apply(HFunctor[W].hmap(ff)(wg)).value)(f)

  def ntabulate[A](f: (F ~> Id) => A): F[A] = ncotraverse[Tabulation, A, A](
    (h: Tabulation[Id, A]) => h(FunctionK.id[Id])
  )(f)
}
object Naperian {
  // Naperian functors are representable in terms of their logarithm
  def asRepresentable[F[_]: Naperian]: Representable.Aux[F, Logarithm[F]] = new Representable[F] {
    override def F: Functor[F] = Naperian[F].F

    override type Representation = Logarithm[F]

    override def index[A](f: F[A]): Representation => A =
      (foo: Representation) => foo.apply(f)

    override def tabulate[A](f: Representation => A): F[A] = Naperian[F].ntabulate[A](f)
  }

  def apply[F[_]: Naperian]: Naperian[F] = implicitly

  def nindex[F[_], A](fa: F[A])(collapse: F ~> Id): A = collapse.apply[A](fa)

  def distributeAlong[F[_]: Naperian, W[?[_], _]: HFunctor, A](w: W[F, A]): F[W[Id, A]] =
    Naperian[F].hdistribute[W].apply(w).value

  def tabulateLog[F[_]: Naperian, A](log: Logarithm[F] => A): F[A] =
    Naperian[F].ntabulate(log)
}

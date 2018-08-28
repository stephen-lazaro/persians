package sl.persians

import cats.{Eval, Functor, Id}
import cats.free.{Cofree, Free}

trait Dual[F[_], G[_]] {
  def zap[A, B, C](f: A => B => C)(fa: F[A])(g: G[B]): C

  def >*<[A, B]: F[A => B] => G[A] => B = zap(identity[A => B])
}
trait Bidual[P[_, _], Q[_, _]] {
  def bizap[A, B, C, D, E](f: A => C => E)(g: B => D => E)(pab: P[A, B])(qcd: Q[C, D]): E

  def >>*<<[A, B, C, D, E]: P[A => C, B => C] => Q[A, B] => C =
    bizap(identity[A => C])(identity[B => C])
}
object Dual {
  def apply[F[_], G[_]](implicit dual: Dual[F, G]): Dual[F, G] = implicitly

  implicit val dualIdId = new Dual[Id, Id] {
    def zap[A, B, C] (f: (A) => (B) => C)(fa: A)(g: B): C = f(fa)(g)
  }

  implicit def dualFunctionProd[C] = new Dual[C => ?, (C, ?)] {
    def zap[A, B, D] (f: A => B => D)(fa: C => A)(g: (C, B)): D = g match {
      case (l, r) => f(fa(l))(r)
    }
  }

  implicit def dualForEvalWithId[F[_], G[_]](
    implicit
    dual: Dual[F, G]
  ): Dual[λ[α => Eval[F[α]]], G] = new Dual[λ[α => Eval[F[α]]], G] {
    def zap[A, B, C](f: A => B => C)(fa: Eval[F[A]])(g: G[B]): C =
      Dual[F, G].zap(f)(fa.value)(g)
  }

  implicit def dualForIdWithEval[F[_], G[_]](
    implicit
    dual: Dual[F, G]
  ): Dual[F, λ[α => Eval[G[α]]]] = new Dual[F, λ[α => Eval[G[α]]]] {
    def zap[A, B, C] (f: A => B => C)(fa: F[A])(g: Eval[G[B]]): C =
      Dual[F, G].zap(f)(fa)(g.value)
  }

  implicit def dualForEvalWithEval[F[_], G[_]](
    implicit
    dual: Dual[F, G]
  ): Dual[λ[α => Eval[F[α]]], λ[α => Eval[G[α]]]] = new Dual[λ[α => Eval[F[α]]], λ[α => Eval[G[α]]]] {
    def zap[A, B, C] (f: A => B => C)(fa: Eval[F[A]])(g: Eval[G[B]]): C =
      Dual[F, G].zap(f)(fa.value)(g.value)
  }

  implicit def dualCofreeFree[F[_], G[_]: Functor](
    implicit
    dual: Dual[F, G],
    bidual: Bidual[Tuple2, Either]
  ) = new Dual[Cofree[F, ?], Free[G, ?]] {
    def zap[A, B, C](f: A => B => C)(cofree: Cofree[F, A])(free: Free[G, B]): C =
      Bidual[Tuple2, Either].bizap(f)(
        Dual[F, G].zap(zap(f)))(
        (cofree.head, cofree.tailForced))(
        free.resume.swap)
  }

  implicit def dualFreeCofree[F[_]: Functor, G[_]](
    implicit
    dual: Dual[F, G],
    bidual: Bidual[Either, Tuple2]
  ) = new Dual[Free[F, ?], Cofree[G, ?]] {
    def zap[A, B, C](f: A => B => C)(free: Free[F, A])(cofree: Cofree[G, B]): C =
      Bidual[Either, Tuple2].bizap(f)(
        Dual[F, G].zap(zap(f)))(
        free.resume.swap)(
        (cofree.head, cofree.tailForced))
  }
}
object Bidual {
  def apply[P[_, _], Q[_, _]](implicit bidual: Bidual[P, Q]): Bidual[P, Q] = implicitly

  implicit val bidualForEitherTuple = new Bidual[Tuple2, Either] {
    def bizap[A, B, C, D, E](f: A => C => E)(g: B => D => E)(pab: (A, B))(qcd: Either[C, D]): E =
      pab match {
        case (l, r) => qcd match {
          case Right(d) => g(r)(d)
          case Left(c) => f(l)(c)
        }
      }
  }

  implicit val bidualForEitherTupleConverse = new Bidual[Either, Tuple2] {
    def bizap[A, B, C, D, E](f: A => C => E)(g: B => D => E)(pab: Either[A, B])(qcd: (C, D)): E =
      qcd match {
        case (c, d) => pab match {
          case Right(b) => g(b)(d)
          case Left(a) => f(a)(c)
        }
      }
  }
}

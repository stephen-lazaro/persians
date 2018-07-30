package sl.persians

import cats.{Applicative, Functor, Id, ~>}

trait Day[F[_], G[_], A] {
  type B
  type C
  def run: (F[B], G[C], (B, C) => A)
}
object Day {
  // Boring very verbose constructor
  def from[F[_], G[_], A, BB, CC](fb: F[BB], gc: G[CC], op: (BB, CC) => A): Day[F, G, A] =
    new Day[F, G, A] {
      type B = BB
      type C = CC
      def run = (fb, gc, op)
    }

  // Bare minimum data
  def apply[F[_], G[_], A, D](fab: F[A => D])(ga: G[A]): Day[F, G, D] =
    new Day[F, G, D] {
      type B = A => D
      type C = A
      def run: (F[this.B], G[this.C], (B, C) => D) = (fab, ga, _.apply(_))
    }

  // Leverage applicative context to collapse values
  def collapse[F[_]: Applicative, A](day: Day[F, F, A]): F[A] =
    day.run match {
      case (fa, fc, combine) => Applicative[F].map2(fa, fc)(combine)
    }

  // Ughhhhh beta reducing this reasonably is gonna be a hassle.
  // too many type variables.
  /**
  def assoc[F[_], G[_], H[_], A](day: Day[F, Day[G, H, ?], A]): Day[Day[F, G, ?], H, A] =
    new Day[Day[F, G, ?], H, A] {
      type B =
      type C =
      def run: (Day[F, G, B], H[C], (B, C) => A) = day.run match {
        case (fb, dayghc, bctoa) =>
          dayghc.match {
            case (gb_, hc_, b_c_toc) =>
              (
                new Day[F, G, B] {

                }
                ???:,
                ???:
              )
          }
      }
    }

  */

  // Push a value into Day combining Id on left
  def leftIntro[F[_], A](fa: F[A]): Day[Id, F, A] =
    new Day[Id, F, A] {
      type B = Unit
      type C = A
      def run: (Id[B], F[C], (B, C) => A) = ((), fa, (_: B, a: C) => a)
    }

  // Push a value into Day combining Id on right
  def rightIntro[F[_], A](fa: F[A]): Day[F, Id, A] =
    new Day[F, Id, A] {
      type B = A
      type C = Unit
      def run: (F[B], Id[C], (B, C) => A) = (fa, (), (a: B, _: C) => a)
    }

  // Drop an Id from a Day to get an F via left
  def leftEliminate[F[_]: Functor, A](dayfa: Day[Id, F, A]): F[A] =
    dayfa.run match {
      case (b, fc, op) => Functor[F].map(fc)((c: dayfa.C) => op(b, c))
    }

  // Drop an Id from a Day to get an F via right
  def rightEliminate[F[_]: Functor, A](dayfa: Day[F, Id, A]): F[A] =
    dayfa.run match {
      case (fb, c, op) => Functor[F].map(fb)((b: dayfa.B) => op(b, c))
    }

  def toCurried[F[_], G[_], H[_], A](trans: Day[F, G, ?] ~> H)(ga: G[A]): Curried[F, H, A] =
    new Curried[F, H, A] {
      def run[R](gar: F[A => R]): H[R] =
        trans(Day.apply(gar)(ga))
    }

  def fromCurried[F[_]: Functor, K[_], H[_], B](trans: K ~> Curried[F, H, ?])(day: Day[F, K, B]): H[B] =
    day.run match {
      case (fb_, kc, op) => trans.apply[day.C](kc).run[B](
        Functor[F].map(fb_)((b: day.B) => op(b, _))
        )
    }

  implicit def functorForDay[F[_], G[_]] = new Functor[Day[F, G, ?]] {
    def map[A, D](fa: Day[F, G, A])(f: A => D): Day[F, G, D] =
      new Day[F, G, D] {
        type B = fa.B
        type C = fa.C
        def run: (F[B], G[C], (B, C) => D) = fa.run match {
          case (fb_inner, gc_inner, op_inner) =>
            (fb_inner, gc_inner, (x: B, y: C) => f(op_inner(x, y)))
        }
      }
  }

  trait Curried[G[_], H[_], A] {
    def run[R](gar: G[A => R]): H[R]
  }
  object Curried {
    implicit def functorForCurriedDay[G[_]: Functor, H[_]] = new Functor[Curried[G, H, ?]] {
      def map[A, B](fa: Curried[G, H, A])(f: A => B): Curried[G, H, B] =
        new Curried[G, H, B] {
          def run[R](gar: G[B => R]): H[R] =
            fa.run(Functor[G].map(gar)(_.compose(f)))
        }
    }
  }
}

package sl.persians

import cats.{Applicative, Apply, Functor, Id, ~>}

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
  def collapse[F[_]: Apply, A](day: Day[F, F, A]): F[A] =
    day.run match {
      case (fa, fc, combine) => Apply[F].map2(fa, fc)(combine)
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

  def swap[F[_], G[_], A](day: Day[F, G, A]): Day[G, F, A] =
    new Day[G, F, A] {
      type B = day.C
      type C = day.B
      def run: (G[B], F[C], (B, C) => A) = day.run match {
        case (fc, gb, op) =>
          (gb, fc, (x: B, y: C) => op(y, x))
      }
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

    def applied[F[_]: Functor, G[_], A](dayCurried: Day[F, Curried[F, G, ?], A]): G[A] =
      dayCurried.run match {
        case (fa, curried, op) =>
          curried.run(Functor[F].map(fa)((b: dayCurried.B) => op(b, _)))
      }

    def unapplied[F[_], G[_], A](ga: G[A]): Curried[F, Day[F, G, ?], A] = new Curried[F, Day[F, G, ?], A] {
      def run[R](far: F[A => R]): Day[F, G, R] = new Day[F, G, R] {
        type B = A => R
        type C = A
        def run: (F[B], G[C], (B, C) => R) = (far, ga, _.apply(_))
      }
    }

    def liftCurried[F[_]: Apply, A](fa: F[A]): Curried[F, F, A] =
      new Curried[F, F, A] {
        def run[R](far: F[A => R]): F[R] = Apply[F].ap(far)(fa)
      }

    def lowerCurried[F[_]: Applicative, A](fa: Curried[F, F, A]): F[A] =
      fa.run(Applicative[F].pure(identity[A]))

    def composeOuter[F[_]: Functor, G[_], H[_], A, B](curriedfgab: Curried[F, G, A => B])(curriedgha: Curried[G, H, A]): Curried[F, H, B] =
      new Curried[F, H, B] {
        def run[R](far: F[B => R]): H[R] =
          curriedgha.run(
            // G[A => R]
            curriedfgab.run(
              Functor[F].map(far)((f: B => R) => (g: A => B) => f.compose(g))
            )
          )
      }
  }
}

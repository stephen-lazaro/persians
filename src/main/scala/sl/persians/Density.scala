package sl.persians

import cats.free.Cofree
import cats.{Applicative, Apply, Comonad, Functor}
import cats.instances.function.catsStdInstancesForFunction1
import cats.syntax.arrow.toArrowOps
import cats.syntax.profunctor.toProfunctorOps
import sl.persians.kan.Lan

// Ugh, need to use the Aux pattern for this to be at all usable...
trait Density[K[_], A] {
  type B
  def run: (K[B] => A, K[B])
  def fi: K[B] = run._2
  def k: K[B] => A = run._1
}
object Density {
  type Aux[G[_], A, B0] = Density[G, A] { type B = B0 }
  def liftToDensity[F[_]: Comonad, A](fa: F[A]): Density[F, A] =
    new Density[F, A] {
      type B = A
      def run: (F[B] => A, F[B]) = (Comonad[F].extract, fa)
    }

  def withCofree[F[_]: Functor, A](a: A)(f: A => F[A]): Density[Cofree[F, ?], A] =
    new Density[Cofree[F, ?], A] {
      type B = A
      def run: (Cofree[F, B] => A, Cofree[F, A]) = (
        Comonad[Cofree[F, ?]].extract[B],
        Cofree.unfold(a)(f)
      )
    }

  def toLan[F[_], A](density: Density[F, A]): Lan[F, F, A] =
    new Lan[F, F, A] {
      type B = density.B
      def run: (F[B] => A, F[B]) = density.run
    }

  // Can this "improve" cofree comonads as lowerCodensity does free monads?
  def lowerDensity[F[_]: Comonad, A](density: Density[F, A]): F[A] =
    density.run match {
      case (f, x) => Comonad[F].coflatMap(x)(f)
    }

  implicit def comonadForDensity[F[_]]: Comonad[Density[F, ?]] = new ComonadForDensity[F] {}

  implicit def applyForDensity[F[_]: Apply]: Apply[Density[F, ?]] = new ApplyForDensity[F] {
    val F = Apply[F]
  }

  implicit def applicativeForDensity[F[_]: Applicative]: Applicative[Density[F, ?]] = new ApplicativeForDensity[F] {
    val F = Applicative[F]
  }

  trait FunctorForDensity[F[_]] extends Functor[Density[F, ?]] {
    def map[A, BB](fa: Density[F, A])(f: A => BB): Density[F, BB] =
      new Density[F, BB] {
        type B = fa.B
        def run: (F[B] => BB, F[B]) = fa.run match {
          case (g, fb) =>
            (g andThen f, fb)
        }
      }
  }

  trait ComonadForDensity[F[_]] extends Comonad[Density[F, ?]] with FunctorForDensity[F] {
    def extract[A](x: Density[F, A]): A = x.run match {
      case (g, fb) => g(fb)
    }

    def coflatMap[A, BB](fa: Density[F, A])(f: Density[F, A] => BB): Density[F, BB] =
      new Density[F, BB] {
        type B = fa.B
        def run: (F[B] => BB, F[B]) = fa.run match {
          case (g, fb) => {
            val composed: F[B] => BB = (fB: F[B]) => f(
              new Density[F, A] {
                type B = fa.B
                def run: (F[B] => A, F[B]) = (g, fB)
              })
            (composed, fb)
          }
        }
      }
  }

  trait ApplyForDensity[F[_]] extends Apply[Density[F, ?]] with FunctorForDensity[F] {
    val F: Apply[F]

    def ap[A, BB](dffa: Density[F, A => BB])(dfa: Density[F, A]): Density[F, BB] =
      new Density[F, BB] {
        type B = (dfa.B, dffa.B)

        def run: (F[B] => BB, F[B]) = (dfa.run, dffa.run) match {
          // ((F[B] => A, F[B]), (F[B] => (A => BB), F[B])
          case ((fb_a, fb), (fb_a_bb, fb_otro)) =>
            val inner: F[B] => BB =
              (F.lift[B, dfa.B](_._1) &&& F.lift (_._2)
                ) rmap
                (((fbl: F[dfa.B]) => fb_a(fbl)) ***
                 ((fbr: F[dffa.B]) => fb_a_bb(fbr))
                ) rmap {
                  case (a, a_to_bb) => a_to_bb(a)
                }
            (inner, F.product (fb, fb_otro))
        }
      }
  }

  trait ApplicativeForDensity[G[_]] extends ApplyForDensity[G] with Applicative[Density[G, ?]]{
    override val F: Applicative[G]
    def pure[A](x: A): Density[G, A] = new Density[G, A] {
      type B = A
      def run: (G[B] => A, G[A]) = (Function.const(x), F.pure(x))
    }
  }
}

package sl.persians

import cats.Functor
/**
 * I believe the deal here is that we are holding a fixed value,
 * and tracking various functional steps through the
 * lattice of typesby composing functions. Think of this
 * as maybe a lazy sort of functional application by
 * just composing all the relevant functions and running them
 * all at once at the end of the universe.
 *
 * In other words, we are accumulating over the functions
 * to be mapped by fmap by restricting composition so that
 * it is a semigroup rather than a category.
 *
 * Note the duality with Yoneda, here we take functions INTO A.
 * There we took functions FROM A.
 * Hence, here we precompose in the functor.
 */
trait Coyoneda [F [_], A] {
  type Relevant
  def run: Relevant => A
  def prior: F [Relevant]
}
object Coyoneda {
  implicit def persiansStdFunctorForCoyoneda [F [_]] =
    new Functor [Coyoneda [F, ?]] {
      def map [A, B] (cya: Coyoneda [F, A])(f: A => B) =
        new Coyoneda [F, B] {
          type Relevant = cya.Relevant
          def run: Relevant => B = f compose cya.run
          def prior: F [Relevant] = cya.prior
        }
    }

    def liCoYo [F [_], A] (fa: F [A]): Coyoneda [F, A] =
      new Coyoneda [F, A] {
        type Relevant = A
        def run: Relevant => A = identity
        def prior: F [A] = fa
      }

    def loCoYo [F [_] : Functor, A] (cya: Coyoneda [F, A]): F [A] =
      implicitly [Functor [F]] .map (cya.prior)(cya.run)
}

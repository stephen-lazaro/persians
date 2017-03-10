package sl.persians

import cats.Functor
/**
 * TBH I am having a great deal of difficulty
 * really grokking this one. But I believe the deal
 * is that we are holding a fixed value, and tracking
 * various functional steps through the lattice of types
 * by composing functions. Think of this as maybe a lazy
 * sort of functional application by just composing all
 * the relevant functions and running them all at once
 * at the end of the universe.
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
}

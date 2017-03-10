package sl.persians

import cats.Functor

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

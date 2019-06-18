package sl.persians

import algebra.{Eq, Monoid, Order}
import cats.Functor
import spire.algebra.{CModule, CRing}

final case class Delta[A](polarity: Polarity, value: A)
object Delta {
  implicit val functorForDelta: Functor[Delta] = new Functor[Delta] {
    def map[A, B](fa: Delta[A])(f: A => B) = Delta(fa.polarity, f(fa.value))
  }

  implicit def moduleForDelta[A: CRing: Order] = new CModule[Delta[A], A] {
    def scalar: CRing[A] = CRing[A]

    // We are enforcing Negative 0 == Positive 0 in the Eq instance
    def zero: Delta[A] = Delta(Polarity.Positive, scalar.zero)

    def negate(x: Delta[A]): Delta[A] = x match {
      case Delta(Polarity.Positive, v) => Delta(Polarity.Negative, v)
      case Delta(Polarity.Negative, v) => Delta(Polarity.Positive, v)
    }

    def plus(x: Delta[A], y: Delta[A]): Delta[A] = (x, y) match {
      case (Delta(Polarity.Positive, v), Delta(Polarity.Positive, w)) =>
        Delta(Polarity.Positive, scalar.plus(v, w))
      case (Delta(Polarity.Negative, v), Delta(Polarity.Negative, w)) =>
        Delta(Polarity.Negative, scalar.plus(v, w))
    }

    def timesl(r: A, v: Delta[A]): Delta[A] = v match {
      // This is where we need the ordering so as to decide
      // if we have to switch polarity to maintain positive magnitude
      // invariance.
      case Delta(polarity, value) =>
        if (Order[A].compare(value, scalar.zero) == 1)
        // Value is "positive" so extend polarity
          Delta(polarity, scalar.times(value, r))
        else if (Order[A].compare(value, scalar.zero) == -1)
        // Value is "negative" so negate polarity
          Delta(Polarity.inverse(polarity), scalar.times(value, scalar.negate(r)))
        else // Value is 0, so it is impossible to scale this vector
          Delta(polarity, value)
    }
  }

  implicit def eqForDelta[A: Monoid: Eq] =  new Eq[Delta[A]] {
    def eqv(x: Delta[A], y: Delta[A]) = (x, y) match {
      case (Delta(Polarity.Positive, v), Delta(Polarity.Positive, w)) =>
        Eq[A].eqv(v, w)
      case (Delta(Polarity.Negative, v), Delta(Polarity.Negative, w)) =>
        Eq[A].eqv(v, w)
      // If they differ in sign, they are equal only if 0 length
      case (Delta(_, v), Delta(_, w)) =>
        Eq[A].eqv(v, Monoid[A].empty) && Eq[A].eqv(w, Monoid[A].empty)
    }
  }
}

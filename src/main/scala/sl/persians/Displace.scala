package sl.persians

import spire.algebra.{CModule, VectorSpace}


trait Displace[V, R] {
  implicit def displacements: CModule[V, R]

  def displace(a: R, v: V): R
}
object Displace {
  implicit def affineFromDeltaVector[A](
    implicit
    deltaSpace: CModule[Delta[A], A]
  ): Displace[Delta[A], A] = new Displace[Delta[A], A] {
    implicit def displacements = deltaSpace

    def displace(a: A, v: Delta[A]): A =
      displacements.plus(Delta(Polarity.Positive, a), v).value
  }

  implicit class affineSyntax[V, A](a: A)(
    implicit
    affineSpace: Displace[V, A]
  ) {
    def /+/(v: V): A = affineSpace.displace(a, v)
  }

}

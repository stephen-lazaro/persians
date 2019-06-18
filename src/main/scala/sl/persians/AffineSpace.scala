package sl.persians

import spire.algebra.{CModule, VectorSpace}


trait AffineSpace[V, R] {
  implicit def displacements: CModule[V, R]

  def displace(a: R, v: V): R
}
object AffineSpace {
  implicit def affineFromDeltaVector[A](
    implicit
    deltaSpace: CModule[Delta[A], A]
  ): AffineSpace[Delta[A], A] = new AffineSpace[Delta[A], A] {
    implicit def displacements = deltaSpace

    def displace(a: A, v: Delta[A]): A =
      displacements.plus(Delta(Polarity.Positive, a), v).value
  }

  implicit class affineSyntax[V, A](a: A)(
    implicit
    affineSpace: AffineSpace[V, A]
  ) {
    def /+/(v: V): A = affineSpace.displace(a, v)
  }

}

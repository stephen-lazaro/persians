package sl.persians

import spire.algebra.AbGroup

trait AffineSpace[P] {
  type Diff
  val abGrp: AbGroup[Diff]

  def difference(p: P, q: P): Diff
}
object AffineSpace {
  type Aux[P, V] = AffineSpace[P] { type Diff = V }
}

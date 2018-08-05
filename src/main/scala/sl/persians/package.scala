package sl

import cats.Id

package object persians {
  type Co[W[_], A] = CoT[W, Id, A]
}

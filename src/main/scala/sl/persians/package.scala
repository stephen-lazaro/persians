import sl.persians.Coyoneda

package object persians {
  type FreeFunctor [F [_]] = Coyoneda [F, _]
}

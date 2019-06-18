package sl.persians

trait Homomorphism[A, B] {
  def render(a: A): B
}
object Homomorphism {
  def apply[A, B](implicit h: Homomorphism[A, B]): Homomorphism[A, B] = h

  implicit class morphismSyntax[A](self: A) {
    def render[B: Homomorphism[A, ?]]: B = Homomorphism[A, B].render(self)
  }
}

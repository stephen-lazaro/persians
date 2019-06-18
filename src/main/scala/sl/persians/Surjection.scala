package sl.persians

trait Surjection[A, B] extends Homomorphism[A, B] {
  /**
    * The law is
    *   render(section(b)) == b
    * but in general
    *   section(render(a)) != a
    */
  def section(b: B): A
}
object Surjection {
  def apply[A, B](implicit h: Surjection[A, B]): Surjection[A, B] = h
}

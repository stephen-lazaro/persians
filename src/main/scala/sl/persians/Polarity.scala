package sl.persians

sealed trait Polarity
object Polarity {
  case object Positive extends Polarity
  case object Negative extends Polarity

  def inverse(polarity: Polarity): Polarity =
    polarity match {
      case Positive => Negative
      case Negative => Positive
    }
}

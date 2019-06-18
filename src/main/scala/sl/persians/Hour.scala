package sl.persians

import libra.UnitOfMeasure
import shapeless.Witness

object time {
  type Time

  type Hour = UnitOfMeasure[Time]
  type Day = UnitOfMeasure[Time]
  type Week = UnitOfMeasure[Time]
}

package sl.persians

import java.time.{Instant, ZoneOffset}

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import spire.algebra.{Action, CModule, CRing}

case class UTCOffset(value: Int Refined Interval.ClosedOpen[W.`0`.T, W.`24`.T])
object UTCOffset {
  type Domain = Interval.ClosedOpen[W.`0`.T, W.`24`.T]

  val UTC = UTCOffset(refineMV[Domain](0))

  implicit val displace: Displace[UTCOffset, Int] = new Displace[UTCOffset, Int] {
    override implicit def displacements: CModule[UTCOffset, Int] = new CModule[UTCOffset, Int] {
      override implicit def scalar: CRing[Int] = CRing[Int]

      override def timesl(r: Int, v: UTCOffset): UTCOffset = v match {
        case UTCOffset(offset) => UTCOffset(Refined.unsafeApply[Int, Domain]((offset.value * r) % 24))
      }

      override def negate(x: UTCOffset): UTCOffset = x match {
        case UTCOffset(value) => UTCOffset(Refined.unsafeApply[Int, Domain]((24 - value.value) % 24))
      }

      override def zero: UTCOffset = UTCOffset(Refined.unsafeApply[Int, Domain](0))

      override def plus(x: UTCOffset, y: UTCOffset): UTCOffset = (x, y) match {
        case (UTCOffset(a), UTCOffset(b)) =>
          UTCOffset(Refined.unsafeApply[Int, Domain]((a + b) % 24))
      }
    }

    override def displace(a: Int, v: UTCOffset): Int = a + v.value.value
  }


  implicit val actionOnInstant: Action[Instant, UTCOffset] = new Action[Instant, UTCOffset] {
    override def actl(p: UTCOffset, g: Instant): Instant = g.atOffset(ZoneOffset.ofHours(p.value.value)).toInstant
    override def actr(g: Instant, p: UTCOffset): Instant = g.atOffset(ZoneOffset.ofHours(p.value.value)).toInstant
  }
}

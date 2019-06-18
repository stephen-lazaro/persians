package sl.persians

import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import shapeless.{Nat, Widen, Witness}
import shapeless.ops.nat.ToInt
import spire.algebra.{CModule, CRing}

case class ZModulo[Upper: Witness.Aux](value: Int Refined Interval.ClosedOpen[W.`0`.T,  Upper])
object ZModulo {
  implicit def module[Upper <: Int](
    implicit
    witness: Witness.Aux[Upper],
    validate: Validate[Int, Interval.ClosedOpen[W.`0`.T,  Upper]],
    widen: Widen.Aux[Upper, Int]
  ): CModule[ZModulo[Upper], Int] =
    if (widen(witness.value) >= 1)
      new CModule[ZModulo[Upper], Int] {
        override implicit def scalar: CRing[Int] = CRing[Int]

        override def timesl(r: Int, v: ZModulo[Upper]): ZModulo[Upper] = v match {
          case ZModulo(value) => ZModulo[Upper](Refined.unsafeApply((r * value.value) % widen(witness.value)))
        }

        override def negate(x: ZModulo[Upper]): ZModulo[Upper] = x match {
          case ZModulo(value) =>
            ZModulo[Upper](Refined.unsafeApply((widen(witness.value) - value) % widen(witness.value)))
        }

        override val zero: ZModulo[Upper] = ZModulo[Upper](refineV(0).right.get)

        override def plus(x: ZModulo[Upper], y: ZModulo[Upper]): ZModulo[Upper] = (x, y) match {
          case (ZModulo(a), ZModulo(b)) =>
            ZModulo[Upper](Refined.unsafeApply((a + b) % widen(witness.value)))
        }
      }
    else throw new Exception("Modulus must be strictly positive")

  implicit def cring[Upper <: Int](
    implicit
    witness: Witness.Aux[Upper],
    validate: Validate[Int, Interval.ClosedOpen[W.`0`.T,  Upper]],
    widen: Widen.Aux[Upper, Int],
  ): CRing[ZModulo[Upper]] =
    if (widen(witness.value) >= 1)
      new CRing[ZModulo[Upper]] {
        override val one: ZModulo[Upper] = ZModulo[Upper](refineV(1).right.get)

        override def plus(x: ZModulo[Upper], y: ZModulo[Upper]): ZModulo[Upper] =
          ZModulo[Upper](
            (x, y) match {
              case (ZModulo(a), ZModulo(b)) =>
                Refined.unsafeApply((a.value + b.value) % widen(witness.value))
            }
          )

        override def times(x: ZModulo[Upper], y: ZModulo[Upper]): ZModulo[Upper] =
          ZModulo[Upper](
            (x, y) match {
              case (ZModulo(a), ZModulo(b)) =>
                Refined.unsafeApply((a.value * b.value) % widen(witness.value))
            }
          )

        override def negate(x: ZModulo[Upper]): ZModulo[Upper] =
          ZModulo[Upper](
            x match {
              case ZModulo(value) =>
                Refined.unsafeApply((24 - value.value) % widen(witness.value))
            }
          )

        override val zero: ZModulo[Upper] = ZModulo[Upper](refineV(0).right.get)
      }
    else throw new Exception("Modulus must be strictly positive.")

  implicit def morphismFromInt[Upper <: Int](
    implicit
    witness: Witness.Aux[Upper],
    validate: Validate[Int, Interval.ClosedOpen[W.`0`.T,  Upper]],
    widen: Widen.Aux[Upper, Int],
  ): Surjection[Int, ZModulo[Upper]] = new Surjection[Int, ZModulo[Upper]] {
    // Summon these eagerly to force errors
    val cmod: CModule[ZModulo[Upper], Int] = CModule[ZModulo[Upper], Int]
    val cring: CRing[ZModulo[Upper]] = CRing[ZModulo[Upper]]

    override def render(a: Int): ZModulo[Upper] =
      cmod.timesl(a, cring.one)

    override def section(b: ZModulo[Upper]): Int =
      b.value.value
  }
}

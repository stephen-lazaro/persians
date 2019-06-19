package sl.persians

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import shapeless.{Widen, Witness}
import singleton.ops.{>=, Require}
import spire.algebra.{CModule, CRing}

final class ZModulo[Upper] private[persians] (val value: Int) extends AnyVal
object ZModulo {
  type Domain[Upper] = Interval.ClosedOpen[W.`0`.T,  Upper]

  def fromRefined[Upper](r: Int Refined Domain[Upper]): ZModulo[Upper] =
    new ZModulo[Upper](r.value)

  def unapply[Upper](arg: ZModulo[Upper]): Option[Int] = Some(arg.value)

  implicit def module[Upper <: Int](
    implicit
    witness: Witness.Aux[Upper],
    widen: Widen.Aux[Upper, Int],
    requirement: Require[Upper >= W.`1`.T]
  ): CModule[ZModulo[Upper], Int] =
      new CModule[ZModulo[Upper], Int] {
        override implicit def scalar: CRing[Int] = CRing[Int]

        override def timesl(r: Int, v: ZModulo[Upper]): ZModulo[Upper] = v match {
          case ZModulo(value) =>
            new ZModulo[Upper]((r * value) % widen(witness.value))
        }

        override def negate(x: ZModulo[Upper]): ZModulo[Upper] = x match {
          case ZModulo(value) =>
            new ZModulo[Upper]((widen(witness.value) - value) % widen(witness.value))
        }

        override val zero: ZModulo[Upper] = new ZModulo[Upper](0)

        override def plus(x: ZModulo[Upper], y: ZModulo[Upper]): ZModulo[Upper] = (x, y) match {
          case (ZModulo(a), ZModulo(b)) =>
            new ZModulo[Upper]((a + b) % widen(witness.value))
        }
      }

  implicit def cring[Upper <: Int](
    implicit
    witness: Witness.Aux[Upper],
    widen: Widen.Aux[Upper, Int],
    requirement: Require[Upper >= W.`1`.T]
  ): CRing[ZModulo[Upper]] =
      new CRing[ZModulo[Upper]] {
        override val one: ZModulo[Upper] = new ZModulo[Upper](1)

        override def plus(x: ZModulo[Upper], y: ZModulo[Upper]): ZModulo[Upper] =
          new ZModulo[Upper](
            (x, y) match {
              case (ZModulo(a), ZModulo(b)) =>
                (a + b) % widen(witness.value)
            }
          )

        override def times(x: ZModulo[Upper], y: ZModulo[Upper]): ZModulo[Upper] =
          new ZModulo[Upper](
            (x, y) match {
              case (ZModulo(a), ZModulo(b)) =>
                (a * b) % widen(witness.value)
            }
          )

        override def negate(x: ZModulo[Upper]): ZModulo[Upper] =
          new ZModulo[Upper](
            x match {
              case ZModulo(value) =>
                (24 - value) % widen(witness.value)
            }
          )

        override val zero: ZModulo[Upper] = new ZModulo[Upper](0)
      }

  // The only way to create ZModulo[Upper] is via the morphism from Int
  implicit def morphismFromInt[Upper <: Int](
    implicit
    witness: Witness.Aux[Upper],
    widen: Widen.Aux[Upper, Int],
    requirement: Require[Upper >= W.`1`.T]
  ): Surjection[Int, ZModulo[Upper]] = new Surjection[Int, ZModulo[Upper]] {
    // Summon these eagerly to force errors
    val cmod: CModule[ZModulo[Upper], Int] = CModule[ZModulo[Upper], Int]
    val cring: CRing[ZModulo[Upper]] = CRing[ZModulo[Upper]]

    override def render(a: Int): ZModulo[Upper] =
      cmod.timesl(a, cring.one)

    override def section(b: ZModulo[Upper]): Int =
      b.value
  }
}

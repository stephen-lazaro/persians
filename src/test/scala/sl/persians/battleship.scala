package sl.persians

import cats.{Functor, Representable}
import cats.syntax.functor.toFunctorOps

object battleship {
  // Adjunction between coordinate functor and board
  // inspired by:
  // https://gist.github.com/ChrisPenner/291038ae1343333fb41523b41181a9d4
  sealed trait Result
  case object Whatever extends Result

  sealed trait Status

  sealed trait Row
  case object I extends Row
  case object II extends Row
  case object III extends Row

  sealed trait Column
  case object A extends Column
  case object B extends Column
  case object C extends Column

  // Would be nice if we could accept different _ways_ of parametrizing space...
  type Indice = (Row, Column)
  final case class CoordF[C, A](coords: C, data: A)
  type BoardF[A] = (
    (A, A, A),
    (A, A, A),
    (A, A, A)
  )

  implicit val functorForBoardF = new Functor[BoardF] {
    def map[A, B](fa: BoardF[A])(f: (A) => B): BoardF[B] = fa match {
      case ((a, b, c), (d, e, h), (x, y, z)) =>
        ((f(a), f(b), f(c)), (f(d), f(e), f(h)), (f(x), f(y), f(z)))
    }
  }

  implicit def functorCoordF[C] = new Functor[CoordF[C, ?]] {
    def map[A, B](fa: CoordF[C, A])(f: A => B): CoordF[C, B] = fa match {
      case CoordF(coords, data) => CoordF(coords, f(data))
    }
  }

  implicit def representableBoard = new Representable[BoardF] {
    def F = Functor[BoardF]

    type Representation = CoordF[Indice, Unit]

    def index[A](f: BoardF[A]): Representation => A = (repr: Representation) => (f, repr) match {
      case (((a, _, _), _, _), CoordF((I, A), _)) => a
      case (((_, b, _), _, _), CoordF((I, B), _)) => b
      case (((_, _, c), _, _), CoordF((I, C), _)) => c
      case ((_, (a, _, _), _), CoordF((II, A), _)) => a
      case ((_, (_, b, _), _), CoordF((II, B), _)) => b
      case ((_, (_, _, c), _), CoordF((II, C), _)) => c
      case ((_, _, (a, _, _)), CoordF((III, A), _)) => a
      case ((_, _, (_, b, _)), CoordF(((III, B)), _)) => b
      case ((_, _, (_, _, c)), CoordF((III, C), _)) => c
    }

    def tabulate[A](f: Representation => A): BoardF[A] = (
      (f(CoordF((I, A), ())), f(CoordF((I, B), ())), f(CoordF((I, C), ()))),
      (f(CoordF((II, A), ())), f(CoordF((II, B), ())), f(CoordF((II, C), ()))),
      (f(CoordF((III, A), ())), f(CoordF((III, B), ())), f(CoordF((III, C), ())))
    )
  }

  implicit def adjunctionBoard = new Adjunction[CoordF[Indice, ?], BoardF] {
    val F: Functor[CoordF[Indice, ?]] = Functor[CoordF[Indice, ?]]
    val U: Representable.Aux[BoardF, CoordF[Indice, Unit]] = representableBoard

    // Populate an empty board into one with the value `a` at every coord
    def unit[A](a: A): BoardF[CoordF[Indice, A]] =
      U.tabulate((coord: CoordF[Indice, Unit]) => coord.as(a))

    // Pull from the board at the index that is designated
    def counit[A](a: CoordF[Indice, BoardF[A]]): A =
      U.index(a.data)(CoordF(a.coords, ()))
  }

  def attack(board: BoardF[Status])(index: Indice): CoordF[Indice, Result] = ???
}

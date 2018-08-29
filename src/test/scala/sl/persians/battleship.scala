package sl.persians

import cats.{Functor, Monad, Monoid, Representable}
import cats.instances.tuple.catsStdMonadForTuple2
import cats.syntax.functor.toFunctorOps

/**
  * Adjunction between coordinate functor and board.
  *
  * Inspired by:
  * https://gist.github.com/ChrisPenner/291038ae1343333fb41523b41181a9d4
  */
object battleship {
  sealed trait Result
  case object Start extends Result
  case object Hit extends Result
  case object Missed extends Result
  case object AlreadyTaken extends Result

  sealed trait Status
  case object Open extends Status
  case object Ship extends Status
  case object Shot extends Status

  sealed trait Row
  case object I extends Row
  case object II extends Row
  case object III extends Row
  object Row {
    def ofString(s: String): Row = s match {
      case "i" => I
      case "ii" => II
      case "iii" => III
      case _ => throw new Error("No good")
    }
  }

  sealed trait Column
  case object A extends Column
  case object B extends Column
  case object C extends Column
  object Column {
    def ofString(s: String): Column = s match {
      case "a" => A
      case "b" => B
      case "c" => C
      case _ => throw new Error("No good")
    }
  }

  // Would be nice if we could accept different _ways_ of parametrizing space...
  type Indice = (Row, Column)
  final case class CoordF[C, A](coords: C, data: A)
  type Coord[A] = CoordF[Indice, A]
  type BoardF[A] = (
    (A, A, A),
    (A, A, A),
    (A, A, A)
  )

  def asCoord(index: Indice): CoordF[Indice, Unit] = CoordF(index, ())

  def indiceBoard: BoardF[Indice] =
    (
      ((I, A), (I, B), (I, C)),
      ((II, A), (II, B), (II, C)),
      ((III, A), (III, B), (III, C))
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

  /**
    * Representable instance for BoardF.
    * The Representation type is Coord[Unit] ~ Indice
    */
  implicit def representableBoard = new Representable[BoardF] {
    def F = functorForBoardF

    type Representation = Coord[Unit]

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

  /**
    * Adjunction between the Coord functor and the
    * Board functor.
    * This allows us to specify ways to build boards
    * entirely in terms of how to handle cells.
    */
  implicit def adjunctionBoard = new Adjunction[Coord, BoardF] {
    val F: Functor[Coord] = Functor[Coord]
    val U: Representable.Aux[BoardF, Coord[Unit]] = representableBoard

    // Populate an empty board into one with the value `a` at every coord
    def unit[A](a: A): BoardF[Coord[A]] =
      U.tabulate((coord: CoordF[Indice, Unit]) => coord.as(a))

    // Pull from the board at the index that is designated
    def counit[A](a: Coord[BoardF[A]]): A =
      U.index(a.data)(asCoord(a.coords))
  }

  /**
    * Zip a board with the indices of it's values using the
    * adjunction specified above.
    */
  def contextualize[A](board: BoardF[A]): BoardF[Coord[A]] =
    Functor[BoardF].map(
      Adjunction.zipRight[Coord, BoardF, Indice, A]((indiceBoard, board)))({
      case (index, data) => CoordF(index, data)
    })

  def atIndex[A](index: Indice)(board: BoardF[A]): A =
    Adjunction[Coord, BoardF].counit(CoordF(index, board))

  def processAttackAt(index: Indice)(coord: Coord[Status]): Unit => Result =
    _ =>
      coord match {
        case CoordF(index_, Open) =>
          if (index_ == index) Missed
          else AlreadyTaken
        case CoordF(index_, Ship) =>
          if (index_ == index) Hit
          else AlreadyTaken
        case CoordF(_, Shot) => AlreadyTaken
      }

  /**
    * Attack a cell and get a result using the adjunction.
    */
  def resultForIndex(index: Indice): BoardF[Coord[Status]] => Coord[Unit] => Result =
    Adjunction.wrapWithAdjunction[Coord, BoardF, Coord[Status], Unit, Result](
      processAttackAt(index)
    )

  /**
    * Produce a new board describing the state of the game.
    */
  def updateBoard(board: BoardF[Coord[Status]])(index: Indice): BoardF[Status] =
    Functor[BoardF].map(board)({
      case CoordF(given, status) =>
        if (given == index) Shot
        else status
    })

  /**
    * Get new result and the new state of the board
    */
  def attack(board: BoardF[Status], index: Indice): (Result, BoardF[Status]) =
    (
      resultForIndex(index)(contextualize(board))(asCoord(index)),
      updateBoard(contextualize(board))(index)
    )

  /**
    * We want to take the newest result, which is the left result.
    */
  implicit val monoidResult: Monoid[Result] = new Monoid[Result] {
    def empty: Result = Start
    def combine(x: Result, y: Result) = {
      println(s"Outcome: $x")
      x
    }
  }

  /**
    * Some crappy IO
    */
  def getIndex: Indice =
    (
      Row.ofString(io.StdIn.readLine("Gimme a Row:, i, ii, iii\n")),
      Column.ofString(io.StdIn.readLine("Gimme a Column: a, b, c\n"))
    )

  /**
    * Predicate that determines game continuance.
    */
  def gameEnds: BoardF[Status] => Boolean = board => Functor[BoardF].map(board)(_ != Ship) match {
    case (
      (true, true, true),
      (true, true, true),
      (true, true, true)
    ) => true
    case _ => false
  }

  /**
    * Iterate turns until the game ends in a tail recursive loop.
    */
  def runGame(board: BoardF[Status]): (Result, BoardF[Status]) =
    Monad[(Result, ?)].iterateUntilM[BoardF[Status]](board)(
      (lboard: BoardF[Status]) => attack(lboard, getIndex))(
      gameEnds
    )
}

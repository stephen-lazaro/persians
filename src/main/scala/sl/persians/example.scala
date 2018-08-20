package sl.persians

import cats.{Id, Monad, MonoidK}
import cats.data.Cokleisli
import cats.free.Cofree
import cats.instances.list.catsStdInstancesForList
import cats.instances.set.catsStdInstancesForSet
import cats.instances.tuple.catsStdInstancesForTuple2
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import cats.syntax.foldable.toFoldableOps
import sl.persians.CoT.monadFromComonad

import scala.util.Random

object example {
  case class Position(x: Int, y: Int)

  type BranchPoint[A] = (Set[Direction], A)
  type Maze[A] = Cofree[BranchPoint, A]
  type Navigation[A] = Co[Maze, A]

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object West extends Direction
  case object South extends Direction
  object Direction {
    def random(): Option[Direction] = {
      val r = new Random().nextInt(5)
      if (r < 1) Some(North)
      else if (r < 2) Some(South)
      else if (r < 3) Some(East)
      else if (r < 4) Some(West)
      else None
    }

    def openFromHere(): Set[Direction] =
      List(random(), random(), random(), random())
        .foldMap(_.toSet[Direction])(MonoidK[Set].algebra[Direction])
  }

  def firstStep: Cokleisli[Maze, Position, Position] =
    Cokleisli[Maze, Position, Position]((m: Maze[Position]) => m.head)

  def step(whereYouAre: Position, direction: String): Position =
    (whereYouAre, direction) match {
      case (Position(x, y), "North") => Position(x, y + 1)
      case (Position(x, y), "South") => Position(x, y - 1)
      case (Position(x, y), "East") => Position(x + 1, y)
      case (Position(x, y), "West") => Position(x - 1, y)
    }

  def takeStep(whereYouAre: Position, set: Set[Direction] = Set.empty[Direction]): Navigation[Position] = {
    val dirsOpen = (if (set.isEmpty) Direction.openFromHere() else set).map(_.toString)
    println(s"You can go: ${dirsOpen}")
    scala.io.StdIn.readLine ("Where do you want to go?\n") match {
      case dir =>
        if (dirsOpen(dir))
          new CoT[Maze, Id, Position] {
            def run[B] (given: Maze[(Position) => B]): B =
              given.map(f => f (step (whereYouAre, dir))).head
          }
        else {
          println("No good. That way is closed.")
          takeStep(whereYouAre)
        }
    }
  }

  def theMaze: Maze[Position] =
    Cofree.unfold[BranchPoint, Position](Position(0, 0))(p => {
      val direction = Direction.random()
      println(direction)
      (direction.toSet, step(p, direction.toString))
    })

  def navigateOnce(maze: Navigation[Position]): Navigation[Position] =
    for {
      place <- maze
      navigation <- takeStep(place)
    } yield navigation

  def keepNavigating(maze: Maze[Position]): Navigation[Position] =
    Monad[Navigation].iterateWhileM(maze.head)(p => takeStep(p))({case Position(x, y) => Math.sqrt(x*x + y*y) < 10})

  def runMaze(): Position =
    keepNavigating(theMaze).run(Cofree.unfold(identity[Position] _)(f => (Set.empty[Direction], f)))
}

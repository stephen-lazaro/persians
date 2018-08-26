package sl.persians

import cats.free.Cofree
import cats.instances.function.catsStdMonadForFunction1
import cats.instances.list.catsStdInstancesForList
import cats.instances.set.catsStdInstancesForSet
import cats.syntax.foldable.toFoldableOps
import cats.{Endo, Id, Monad, MonoidK}
import sl.persians.CoT.monadFromComonad

import scala.util.Random

object maze {
  case class Position(x: Int, y: Int)
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

  // Every node takes a direction and returns an A
  type BranchPoint[A] = Direction => A
  // Maze is a tree of transitions from nodes along directions
  type Maze[A] = Cofree[BranchPoint, A] // Fix[(A, BranchPoint[A])]
  // The Monad obtained from selecting in CleanMaze
  type Navigation[A] = Co[Maze, A]

  def step(whereYouAre: Position, direction: String): Position =
    (whereYouAre, direction) match {
      case (Position(x, y), "North") => Position(x, y + 1)
      case (Position(x, y), "South") => Position(x, y - 1)
      case (Position(x, y), "East") => Position(x + 1, y)
      case (Position(x, y), "West") => Position(x - 1, y)
    }

  // Kleisli[Navigation, Position, Position]
  def takeStep(whereYouAre: Position, set: Set[Direction] = Set.empty[Direction]): Navigation[Position] = {
    val dirsOpen = (if (set.isEmpty) Direction.openFromHere() else set).map(_.toString)
    println(s"You can go in any of: $dirsOpen")
    scala.io.StdIn.readLine ("Where do you want to go?\n") match {
      case dir =>
        if (dirsOpen(dir))
          new CoT[Maze, Id, Position] {
            def run[BB](given: Maze[Position => BB]): BB =
              given.map((f: Position => BB) => f(step(whereYouAre, dir))).head
          }
        else {
          println("No good. That way is closed.")

          takeStep(whereYouAre, set)
        }
    }
  }

  def theMaze: Maze[Position] =
    Cofree.unfold[BranchPoint, Position](Position(0, 0))((p: Position) =>
      (d: Direction) => step(p, d.toString)
    )

  def keepNavigating(radius: Double)(maze: Maze[Position]): Navigation[Position] =
    // Bound the monadic recursion by declaring the Maze of finite radius
    // `Co` not currently stack safe...
    Monad[Navigation].iterateWhileM(maze.head)(takeStep(_, Set.empty[Direction]))({
        case Position(x, y) => Math.sqrt(x*x + y*y) < radius
      })

  def runMaze(): Position =
    keepNavigating(10)(theMaze).run(
      // Construct a tree of ways to transition the node at that point in the value tree.
      // Given a transition, takes a step in the requested direction and then applies it.
      Cofree.unfold[BranchPoint, Endo[Position]](identity[Position])(
        (transition: Position => Position) =>
          (direction: Direction) =>
            (point: Position) =>
              transition(step(point, direction.toString))))
}

package sl.persians

import cats.{Functor, Id}
import cats.instances.function.catsStdMonadForFunction1
import cats.instances.list.catsStdInstancesForList
import cats.free.{Cofree, Free}
import sl.persians.Dual._

object dualConsumption {
  type Nat[A] = Free[Id, A]
  type Stream[A] = Cofree[Id, A]

  def consume[F[_]: Functor, G[_], A, B, C](f: A => B => C)(free: Free[F, A])(cofree: Cofree[G, B])(
    implicit
    dual: Dual[F, G]
  ): C = Dual[Free[F, ?], Cofree[G, ?]].zap(f)(free)(cofree)

  def indexAt[A, B](nat: Nat[A])(stream: Stream[B]): (A, B) =
    consume((Tuple2.apply[A, B] _).curried)(nat)(stream)

  implicit val unsafeSelfDualForList: Dual[List, List] = new Dual[List, List] {
    def zap[A, B, C](f: A => B => C)(fa: List[A])(g: List[B]): C =
      fa.zip(g).map(Function.uncurried(f).tupled).head
  }

  type Path[A] = Free[List, A]
  type RoseTree[A] = Cofree[List, A]

  def leafVia[A, B](path: Path[A])(tree: RoseTree[B]): (A, B) =
    consume((Tuple2.apply[A, B] _).curried)(path)(tree)

  type Transitions[C, A] = Free[C => ?, A]
  type MooreMachine[C, A] = Cofree[(C, ?), A]

  def throughTransitions[A, B, C](transitions: Transitions[C, A])(machine: MooreMachine[C, B]): (A, B) =
    consume((Tuple2.apply[A, B] _).curried)(transitions)(machine)
}

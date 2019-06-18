package sl.persians.auth

//import cats.arrow.FunctionK
//import cats.data.Nested
//import cats.{Applicative, Apply, Comonad, Distributive, Eval, Functor, Monad, MonadError, Traverse, ~>}
//import com.twitter.util.Future
//import io.finch.Endpoint.Result
//import io.finch.EndpointResult.NotMatched
//import io.finch.{Endpoint, EndpointResult, Input, Output}
//import shapeless.{:+:, CNil}
//
//trait Cosemigroupal[F[_]] {
//  def coproduct[A, B](eit: Either[F[A], F[B]]): F[Either[A, B]]
//}
//
//sealed trait Auth[A] {
//  val value: A
//}
//object Auth {
//  // We can get a lot of instances because it's isomorphic to Id, minus the Monad outside this file.
//  implicit def authComonad: Comonad[Auth] with Apply[Auth] with Cosemigroupal[Auth] with Distributive[Auth] with Traverse[Auth] =
//    new Comonad[Auth] with Apply[Auth] with Cosemigroupal[Auth] with Distributive[Auth] with Traverse[Auth] {
//      def extract[A](fa: Auth[A]): A = fa.value
//      def coflatMap[A, B](fa: Auth[A])(f: Auth[A] => B): Auth[B] = new Auth[B] {
//        val value = f(fa)
//      }
//      override def map[A, B](fa: Auth[A])(f: A => B): Auth[B] = new Auth[B] {
//        val value = f(fa.value)
//      }
//      override def ap[A, B](ff: Auth[A => B])(fa: Auth[A]): Auth[B] = new Auth[B] {
//        val value = ff.value(fa.value)
//      }
//      override def coproduct[A, B](eit: Either[Auth[A], Auth[B]]): Auth[Either[A, B]] = eit match {
//        case Right(auth) => new Auth[Either[A, B]] {
//          val value = Right(auth.value)
//        }
//        case Left(auth) => new Auth[Either[A, B]] {
//          val value = Left(auth.value)
//        }
//      }
//      override def distribute[G[_], A, B](ga: G[A])(f: A => Auth[B])(implicit evidence$1: Functor[G]): Auth[G[B]] =
//        new Auth[G[B]] {
//          override val value: G[B] = Functor[G].map(ga)(f andThen (_.value))
//        }
//
//      override def traverse[G[_], A, B](fa: Auth[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Auth[B]] =
//        Functor[G].map(f(fa.value))(x => new Auth[B] {
//          val value = x
//        })
//
//      override def foldLeft[A, B](fa: Auth[A], b: B)(f: (B, A) => B): B = f(b, fa.value)
//
//      override def foldRight[A, B](fa: Auth[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa.value, lb)
//    }
//}
//
//trait AuthedEndpoint[F[_], G[_], B] {
//  val endpoint: Endpoint[Nested[F, G, ?], B]
//}
//object AuthedEndpoint {
//  implicit class endpointOp[F[_], A](e: Endpoint[F, A]) {
//    def shiftK[G[_]](f: F ~> G): Endpoint[G, A] = new Endpoint[G, A] {
//      override def apply(input: Input): Result[G, A] = e(input) match {
//        case a @ EndpointResult.Matched(rem, trc, out) => EndpointResult.Matched(rem, trc, f(out))
//        case a @ EndpointResult.NotMatched => EndpointResult.NotMatched[G]
//      }
//    }
//
//    def shiftLocal[G[_], B](f: F[Output[A]] => G[Output[B]]): Endpoint[G, B] = new Endpoint[G, B] {
//      override def apply(input: Input): Result[G, B] = e(input) match {
//        case a @ EndpointResult.Matched(rem, trc, out) => EndpointResult.Matched(rem, trc, f(out))
//        case a @ EndpointResult.NotMatched => EndpointResult.NotMatched[G]
//      }
//    }
//  }
//  implicit val functorForOutput: Functor[Output] = new Functor[Output] {
//    override def map[A, B](fa: Output[A])(f: A => B): Output[B] = fa.map(f)
//  }
//  def make[F[_]: Monad, G[_]: Comonad: Traverse, AA, B](path: Endpoint[F, G[AA]])(f: G[AA] => F[B]): AuthedEndpoint[F, G, B] =
//    new AuthedEndpoint[F, G, B] {
//      val endpoint: Endpoint[Nested[F, G, ?], B] =
//        path.shiftLocal[Nested[F, G, ?], B](???)
//      }
//
//  // Need to think about flattening coproducts here.
//  def :+:[F[_]: MonadError[?[_], Throwable], G[_], B, C](left: AuthedEndpoint[F, G, B], right: AuthedEndpoint[F, G, C]): AuthedEndpoint[F, G,  B :+: C :+: CNil] =
//    new AuthedEndpoint[F, G, B :+: C :+: CNil] {
//      val endpoint: Endpoint[Nested[F, G, ?], B :+: C :+: CNil] = left.endpoint :+: right.endpoint
//    }
//}

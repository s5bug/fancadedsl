package tf.bug.fancadedsl

import cats._
import cats.data.StateT
import cats.implicits._
import cats.kernel.Monoid
import polymorphic.Sigma
import shapeless.{HList, Nat}

case class World(blocks: Set[Sigma[Fancade.Block[_, _ <: HList, _ <: HList, _ <: Boolean, _ <: Nat], Show.ContravariantShow]], connections: Set[Sigma[Fancade.Connection[_, _], Show.ContravariantShow]])

object World {

  trait Implicits {

    implicit class BlockOps[F[_]: Applicative, A, I <: HList, O <: HList, E <: Boolean, X <: Nat](block: F[Fancade.Block[A, I, O, E, X]])(implicit showA: Show[A]) {

      def ! : StateT[F, World, Fancade.Block[A, I, O, E, X]] = StateT[F, World, Fancade.Block[A, I, O, E, X]](world => block.map(b => (world.copy(blocks = world.blocks + b), b)))

    }

    implicit class ConnectionOps[F[+_]: Applicative, C <: Fancade.Connection[_, _]](connection: F[C])(implicit showC: Show[C]) {

      def ! : StateT[F, World, C] = StateT[F, World, C](world => connection.map(c => (world.copy(connections = world.connections + c), c)))

    }

    implicit class PureOps[F[_]: Applicative, A](fa: F[A]) {

      def ? : StateT[F, World, A] = StateT.liftF[F, World, A](fa)

    }

    implicit val worldMonoid: Monoid[World] = new Monoid[World] {
      override def empty: World = World(Set.empty, Set.empty)

      override def combine(x: World, y: World): World = (x, y) match {
        case (World(ba, ca), World(bb, cb)) => World(ba ++ bb, ca ++ cb)
      }
    }

    implicit val showWorld: Show[World] = Show.show {
      case World(blocks, connections) =>
        "- Blocks:\n" ++
          blocks.map(s => s.second.show(s.first)).mkString("\n") ++
          "\n\n- Connections:\n" ++
          connections.map(s => s.second.show(s.first)).mkString("\n")
    }

  }

}

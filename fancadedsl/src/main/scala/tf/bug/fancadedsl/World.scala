package tf.bug.fancadedsl

import cats._
import cats.implicits._
import cats.kernel.Monoid
import polymorphic.{Instance, Sigma}
import shapeless.{HList, Nat}

case class World(blocks: Vector[Sigma[Fancade.Block[_, _ <: HList, _ <: HList, _ <: Boolean, _ <: Nat], Show.ContravariantShow]], connections: Vector[Sigma[Fancade.Connection[_, _], Show.ContravariantShow]])

object World {

  trait Implicits {

    implicit val worldMonoid: Monoid[World] = new Monoid[World] {
      override def empty: World = World(Vector.empty, Vector.empty)

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

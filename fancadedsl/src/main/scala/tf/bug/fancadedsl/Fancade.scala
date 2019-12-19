package tf.bug.fancadedsl

import cats._
import cats.effect.Sync
import cats.implicits._
import io.chrisdavenport.fuuid.FUUID
import shapeless._
import shapeless.ops.hlist.At
import shapeless.ops.hlist.At.Aux
import shapeless.ops.nat.ToInt
import tf.bug.fancadedsl.Fancade.{Block, DataConnection}

trait Fancade[F[_]] {

  def value[T: DataType](t: T): F[Value[T]]
  def variable[T: DataType]: F[Variable[T]]

  def block[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](definition: A)(
      implicit ev: AsBlock.Aux[A, I, O, E, X]
  ): F[Block[A, I, O, E, X]]
  def connect[T, A, B, O <: HList, I <: HList, G <: Nat, S <: Nat](
      from: Block[A, _ <: HList, O, _ <: Boolean, _ <: Nat],
      getting: G,
      to: Block[B, I, _ <: HList, _ <: Boolean, _ <: Nat],
      setting: S
  )(
      implicit outputTypeEv: At.Aux[O, G, T],
      inputTypeEv: At.Aux[I, S, T],
      dataType: DataType[T],
      outputIndexEv: ToInt[G],
      inputIndexEv: ToInt[S]
  ): F[DataConnection[T, A, B, O, I, G, S]]

}

object Fancade {

  def apply[F[_]: Sync]: Fancade[F] = new Fancade[F] {

    override def value[T: DataType](t: T): F[Value[T]] = Value(t).pure[F]
    override def variable[T: DataType]: F[Variable[T]] = Variable[F, T]

    override def block[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
        definition: A
    )(implicit ev: AsBlock.Aux[A, I, O, E, X]): F[Block[A, I, O, E, X]] =
      FUUID.randomFUUID.map(Block(definition, _))

    override def connect[T, A, B, O <: HList, I <: HList, G <: Nat, S <: Nat](
        from: Block[A, _ <: HList, O, _ <: Boolean, _ <: Nat],
        getting: G,
        to: Block[B, I, _ <: HList, _ <: Boolean, _ <: Nat],
        setting: S
    )(
        implicit outputTypeEv: Aux[O, G, T],
        inputTypeEv: Aux[I, S, T],
        dataType: DataType[T],
        outputIndexEv: ToInt[G],
        inputIndexEv: ToInt[S]
    ): F[DataConnection[T, A, B, O, I, G, S]] =
      DataConnection(from, getting, to, setting).pure[F]

  }

  case class Block[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
      definition: A,
      id: FUUID
  )

  case class DataConnection[T, A, B, O <: HList, I <: HList, G <: Nat, S <: Nat](
      from: Block[A, _ <: HList, O, _ <: Boolean, _ <: Nat],
      getting: G,
      to: Block[B, I, _ <: HList, _ <: Boolean, _ <: Nat],
      setting: S
  )(
      implicit outputTypeEv: At.Aux[O, G, T],
      inputTypeEv: At.Aux[I, S, T],
      dataType: DataType[T],
      outputIndexEv: ToInt[G],
      inputIndexEv: ToInt[S]
  ) {
    val fromIndex: Int = outputIndexEv()
    val toIndex: Int = inputIndexEv()
  }

  trait Implicits {

    implicit def showBlock[A](
        implicit showDefinition: Show[A]
    ): Show[Block[A, _ <: HList, _ <: HList, _ <: Boolean, _ <: Nat]] =
      Show.show(block => show"Block(${block.definition} @ ${block.id})")

    implicit def showDataConnection[T, A, B](
        implicit dataType: DataType[T],
        showA: Show[A],
        showB: Show[B]
    ): Show[
      DataConnection[T, A, B, _ <: HList, _ <: HList, _ <: Nat, _ <: Nat]
    ] =
      Show.show(connection =>
        show"Connection[${dataType.name}](${connection.fromIndex} @ ${connection.from} -> ${connection.toIndex} @ ${connection.to})"
      )

  }

}

package tf.bug.fancadedsl

import cats._
import cats.effect.Sync
import cats.implicits._
import io.chrisdavenport.fuuid.FUUID
import shapeless._
import shapeless.ops.hlist.At
import shapeless.ops.hlist.At.Aux
import shapeless.ops.nat._
import tf.bug.fancadedsl.Fancade.{
  Block,
  BranchConnection,
  DataConnection,
  EffectConnection
}

trait Fancade[F[_]] {

  def value[T: DataType](t: T): F[Value[T]]
  def variable[T: DataType]: F[Variable[T]]

  def block[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](definition: A)(
      implicit ev: AsBlock.Aux[A, I, O, E, X]
  ): F[Block[A, I, O, E, X]]
  def connect[From, To, A, B, O <: HList, I <: HList, G <: Nat, S <: Nat](
      from: Block[A, _ <: HList, O, _ <: Boolean, _ <: Nat],
      getting: G,
      to: Block[B, I, _ <: HList, _ <: Boolean, _ <: Nat],
      setting: S
  )(
      implicit outputTypeEv: Aux[O, G, From],
      inputTypeEv: Aux[I, S, To],
      dataTypeFrom: DataType[From],
      dataTypeTo: DataType[To],
      outputIndexEv: ToInt[G],
      inputIndexEv: ToInt[S],
      connectable: Connectable.Aux[From, To, true]
  ): F[DataConnection[From, To, A, B, O, I, G, S]]
  def to[A, B](
      from: Block[A, _ <: HList, _ <: HList, true, _ <: Nat],
      to: Block[B, _ <: HList, _ <: HList, true, _ <: Nat]
  ): F[EffectConnection[A, B]]
  def on[A, B, X <: Nat, Y <: Nat](
      from: Block[A, _ <: HList, _ <: HList, _ <: Boolean, X],
      at: Y,
      to: Block[B, _ <: HList, _ <: HList, true, _ <: Nat]
  )(
      implicit validBranchEv: LT[Y, X],
      atIndexEv: ToInt[Y]
  ): F[BranchConnection[A, B, X, Y]]

}

object Fancade {

  def apply[F[_]: Sync]: Fancade[F] = new Fancade[F] {

    override def value[T: DataType](t: T): F[Value[T]] = Value(t).pure[F]
    override def variable[T: DataType]: F[Variable[T]] = Variable[F, T]

    override def block[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
        definition: A
    )(implicit ev: AsBlock.Aux[A, I, O, E, X]): F[Block[A, I, O, E, X]] =
      FUUID.randomFUUID.map(Block(definition, _))

    override def connect[
        From,
        To,
        A,
        B,
        O <: HList,
        I <: HList,
        G <: Nat,
        S <: Nat
    ](
        from: Block[A, _ <: HList, O, _ <: Boolean, _ <: Nat],
        getting: G,
        to: Block[B, I, _ <: HList, _ <: Boolean, _ <: Nat],
        setting: S
    )(
        implicit outputTypeEv: Aux[O, G, From],
        inputTypeEv: Aux[I, S, To],
        dataTypeFrom: DataType[From],
        dataTypeTo: DataType[To],
        outputIndexEv: ToInt[G],
        inputIndexEv: ToInt[S],
        connectable: Connectable.Aux[From, To, true]
    ): F[DataConnection[From, To, A, B, O, I, G, S]] =
      DataConnection(from, getting, to, setting).pure[F]

    override def to[A, B](
        from: Block[A, _ <: HList, _ <: HList, true, _ <: Nat],
        to: Block[B, _ <: HList, _ <: HList, true, _ <: Nat]
    ): F[EffectConnection[A, B]] = EffectConnection(from, to).pure[F]

    override def on[A, B, X <: Nat, Y <: Nat](
        from: Block[A, _ <: HList, _ <: HList, _ <: Boolean, X],
        at: Y,
        to: Block[B, _ <: HList, _ <: HList, true, _ <: Nat]
    )(
        implicit validBranchEv: LT[Y, X],
        atIndexEv: ToInt[Y]
    ): F[BranchConnection[A, B, X, Y]] = BranchConnection(from, at, to).pure[F]

  }

  case class Block[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
      definition: A,
      id: FUUID
  )

  sealed trait Connection[A, B]
  case class DataConnection[
      F,
      T,
      A,
      B,
      O <: HList,
      I <: HList,
      G <: Nat,
      S <: Nat
  ](
      from: Block[A, _ <: HList, O, _ <: Boolean, _ <: Nat],
      getting: G,
      to: Block[B, I, _ <: HList, _ <: Boolean, _ <: Nat],
      setting: S
  )(
      implicit outputTypeEv: At.Aux[O, G, F],
      inputTypeEv: At.Aux[I, S, T],
      dataTypeFrom: DataType[F],
      dataTypeTo: DataType[T],
      outputIndexEv: ToInt[G],
      inputIndexEv: ToInt[S],
      connectable: Connectable.Aux[F, T, true]
  ) extends Connection[A, B] {
    val fromIndex: Int = outputIndexEv()
    val toIndex: Int = inputIndexEv()
  }

  case class EffectConnection[A, B](
      from: Block[A, _ <: HList, _ <: HList, true, _ <: Nat],
      to: Block[B, _ <: HList, _ <: HList, true, _ <: Nat]
  ) extends Connection[A, B]

  case class BranchConnection[A, B, X <: Nat, Y <: Nat](
      from: Block[A, _ <: HList, _ <: HList, _ <: Boolean, X],
      at: Y,
      to: Block[B, _ <: HList, _ <: HList, true, _ <: Nat]
  )(
      implicit validBranchEv: LT[Y, X],
      atIndexEv: ToInt[Y]
  ) extends Connection[A, B] {
    val atIndex: Int = atIndexEv()
  }

  trait Implicits {

    implicit def showBlock[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
        implicit showDefinition: Show[A]
    ): Show[Block[A, I, O, E, X]] =
      Show.show(block => show"Block(${block.definition} @ ${block.id})")

    implicit def showDataConnection[
        F,
        T,
        A,
        B,
        O <: HList,
        I <: HList,
        G <: Nat,
        S <: Nat
    ](
        implicit dataTypeFrom: DataType[F],
        dataTypeTo: DataType[T],
        showA: Show[A],
        showB: Show[B]
    ): Show[
      DataConnection[F, T, A, B, O, I, G, S]
    ] =
      Show.show(connection =>
        show"Connection[${dataTypeFrom.name} -> ${dataTypeTo.name}](${connection.fromIndex} @ ${connection.from} -> ${connection.toIndex} @ ${connection.to})"
      )

    implicit def showEffectConnection[
        A,
        B
    ](
        implicit showA: Show[A],
        showB: Show[B]
    ): Show[EffectConnection[A, B]] =
      Show.show(connection =>
        show"Connection[effect](${connection.from} -> ${connection.to})"
      )

    implicit def showBranchConnection[
        A,
        B,
        X <: Nat,
        Y <: Nat
    ](
        implicit showA: Show[A],
        showB: Show[B]
    ): Show[BranchConnection[A, B, X, Y]] =
      Show.show(connection =>
        show"Connection[branch](${connection.from} @ ${connection.atIndex} -> ${connection.to})"
      )

  }

}

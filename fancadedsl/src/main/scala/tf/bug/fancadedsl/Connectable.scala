package tf.bug.fancadedsl

import shapeless._
import shapeless.ops.hlist.At

trait BlocksConnectable[
    Outputs <: HList,
    OutputIndex <: Nat,
    Inputs <: HList,
    InputIndex <: Nat
] {

  type Out <: Boolean

}

object BlocksConnectable {

  type Aux[
      Outputs <: HList,
      OutputIndex <: Nat,
      Inputs <: HList,
      InputIndex <: Nat,
      R <: Boolean
  ] =
    BlocksConnectable[Outputs, OutputIndex, Inputs, InputIndex] { type Out = R }

  def apply[
      Outputs <: HList,
      OutputIndex <: Nat,
      Inputs <: HList,
      InputIndex <: Nat
  ](
      implicit e: BlocksConnectable[Outputs, OutputIndex, Inputs, InputIndex]
  ): Aux[Outputs, OutputIndex, Inputs, InputIndex, e.Out] = e

  trait Implicits {

    implicit def blocksConnectable[
        Outputs <: HList,
        OutputIndex <: Nat,
        Inputs <: HList,
        InputIndex <: Nat,
        OutputElement,
        InputElement
    ](
        implicit outputElementEv: At.Aux[Outputs, OutputIndex, OutputElement],
        inputElementEv: At.Aux[Inputs, InputIndex, InputElement],
        connectable: Connectable[OutputElement, InputElement]
    ): BlocksConnectable.Aux[
      Outputs,
      OutputIndex,
      Inputs,
      InputIndex,
      connectable.Out
    ] = new BlocksConnectable[Outputs, OutputIndex, Inputs, InputIndex] {
      override type Out = connectable.Out
    }

  }

}

trait Connectable[From, To] {

  type Out <: Boolean

}

object Connectable {

  type Aux[From, To, R <: Boolean] = Connectable[From, To] { type Out = R }

  def apply[From, To](implicit e: Connectable[From, To]): Aux[From, To, e.Out] =
    e

  trait Implicits {

    implicit def connectableInherits[From, To](
        implicit ev: From <:< To
    ): Aux[From, To, true] =
      new Connectable[From, To] {
        override type Out = true
      }

    implicit def notConnectable[From, To](
        implicit ev: From <:!< To
    ): Aux[From, To, false] =
      new Connectable[From, To] {
        override type Out = false
      }

  }

}

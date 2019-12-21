package tf.bug.fancadedsl

import shapeless.{HList, Nat}

trait AsBlock[A] {

  type IsEffect <: Boolean
  type ExtraEffects <: Nat

  type Inputs <: HList
  type Outputs <: HList

}

object AsBlock {

  type Aux[A, I <: HList, O <: HList, E <: Boolean, X <: Nat] = AsBlock[A] {
    type IsEffect = E
    type ExtraEffects = X
    type Inputs = I
    type Outputs = O
  }

}

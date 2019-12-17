package tf.bug.fancadedsl

import shapeless._
import shapeless.ops.hlist.At

trait IndicesEqual[A <: HList, I <: Nat, B <: HList, J <: Nat] {

  type Out

}

object IndicesEqual {

  type Aux[A <: HList, I <: Nat, B <: HList, J <: Nat, T] =
    IndicesEqual[A, I, B, J] { type Out = T }

  def apply[A <: HList, I <: Nat, B <: HList, J <: Nat](
      implicit e: IndicesEqual[A, I, B, J]
  ): Aux[A, I, B, J, e.Out] = e

}

trait TypesEqual[A, B] {

  type Out <: Boolean

}

object TypesEqual {

  type Aux[A, B, T <: Boolean] = TypesEqual[A, B] { type Out = T }

  def apply[A, B](implicit e: TypesEqual[A, B]): Aux[A, B, e.Out] = e

}

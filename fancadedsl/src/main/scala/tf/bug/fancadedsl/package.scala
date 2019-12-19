package tf.bug

import shapeless.{nat, _}
import shapeless.nat._
import shapeless.ops.hlist.{At, Length}
import tf.bug.fancadedsl.Logic.IfThenElse

package object fancadedsl
    extends Fancade.Implicits
    with DataType.Implicits
    with Variable.Implicits
    with Value.Implicits
    with NumberMath.Implicits
    with Logic.Implicits {

  implicit def hlistIndexesEqual[
      A <: HList,
      I <: Nat,
      B <: HList,
      J <: Nat,
      AT,
      BT,
      R <: Boolean
  ](
      implicit ae: At.Aux[A, I, AT],
      be: At.Aux[B, J, BT],
      ev: TypesEqual.Aux[AT, BT, R]
  ): IndicesEqual.Aux[A, I, B, J, R] = new IndicesEqual[A, I, B, J] {

    override type Out = R

  }

  implicit def equalEqual[A, B](
      implicit ev: A =:= B
  ): TypesEqual.Aux[A, B, true] = new TypesEqual[A, B] {

    override type Out = true

  }

  implicit def unequalEqual[A, B](
      implicit ev: A =:!= B
  ): TypesEqual.Aux[A, B, false] = new TypesEqual[A, B] {

    override type Out = false

  }

}

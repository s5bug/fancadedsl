package tf.bug.fancadedsl

import shapeless._
import shapeless.nat._
import shapeless.ops.hlist.Length

case class Variable[T: DataType](name: String)

object Variable {

  case class Get[T: DataType](v: Variable[T])

  case class Set[T: DataType](v: Variable[T])

  trait Implicits {

    implicit def getVariableBlock[T: DataType]
        : ScriptBlock.Aux[Get[T], HNil, T :: HNil, false, _0] =
      new ScriptBlock[Get[T]] {
        override type IsEffect = false
        override type ExtraEffects = _0
        override type Inputs = HNil
        override type Outputs = T :: HNil
      }

    implicit def setVariableBlock[T: DataType]
        : ScriptBlock.Aux[Set[T], T :: HNil, HNil, true, _0] =
      new ScriptBlock[Set[T]] {
        override type IsEffect = true
        override type ExtraEffects = _0
        override type Inputs = T :: HNil
        override type Outputs = HNil
      }

  }

}

case class Value[T: DataType](value: T)

object Value {

  case class Get[T: DataType](v: Value[T])

  trait Implicits {

    implicit def getValueBlock[T: DataType]
        : ScriptBlock.Aux[Get[T], HNil, T :: HNil, false, _0] =
      new ScriptBlock[Get[T]] {
        override type IsEffect = false
        override type ExtraEffects = _0
        override type Inputs = HNil
        override type Outputs = T :: HNil
      }

  }

}

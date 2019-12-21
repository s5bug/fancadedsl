package tf.bug.fancadedsl

import cats.Show
import cats.effect.Sync
import cats.implicits._
import shapeless._
import shapeless.nat._

import scala.util.Random

case class Variable[T: DataType](name: String)

object Variable {

  def apply[F[_], T](
      implicit sync: Sync[F],
      dataType: DataType[T]
  ): F[Variable[T]] =
    sync
      .delay {
        val bits = 72
        val id = BigInt(bits, Random)
        s"${dataType.name.head}${id.toString(36)}"
      }
      .map(Variable[T](_))

  case class Get[T: DataType](variable: Variable[T])
  case class Set[T: DataType](variable: Variable[T])

  trait Implicits {

    implicit def showVariable[T](
        implicit dataType: DataType[T]
    ): Show[Variable[T]] =
      Show.show(v => {
        show"Variable[${dataType.name}](${v.name})"
      })

    implicit def showGetVariable[T](
        implicit dataType: DataType[T]
    ): Show[Get[T]] = Show.show {
      case Get(v) => show"Get[${dataType.name}]($v)"
    }

    implicit def getVariableAsBlock[T: DataType]
        : AsBlock.Aux[Get[T], HNil, Quoted[T] :: HNil, false, _0] =
      new AsBlock[Get[T]] {
        override type IsEffect = false
        override type ExtraEffects = _0
        override type Inputs = HNil
        override type Outputs = Quoted[T] :: HNil
      }

    implicit def showSetVariable[T](
        implicit dataType: DataType[T]
    ): Show[Set[T]] = Show.show {
      case Set(v) => show"Set[${dataType.name}]($v)"
    }

    implicit def setVariableAsBlock[T: DataType]
        : AsBlock.Aux[Set[T], T :: HNil, HNil, true, _0] = new AsBlock[Set[T]] {
      override type IsEffect = true
      override type ExtraEffects = _0
      override type Inputs = T :: HNil
      override type Outputs = HNil
    }

  }

}

case class Value[T: DataType](value: T)

object Value {

  trait Implicits {

    implicit def showValue[T](
        implicit dataType: DataType[T],
        showValue: Show[T]
    ): Show[Value[T]] =
      Show.show(v => {
        show"Value[${dataType.name}](${v.value})"
      })

    implicit def valueAsBlock[T: DataType]
        : AsBlock.Aux[Value[T], HNil, T :: HNil, false, _0] =
      new AsBlock[Value[T]] {
        override type IsEffect = false
        override type ExtraEffects = _0
        override type Inputs = HNil
        override type Outputs = T :: HNil
      }

  }

}

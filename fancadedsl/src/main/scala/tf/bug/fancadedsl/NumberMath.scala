package tf.bug.fancadedsl

import cats.Show
import shapeless._

object NumberMath {

  case object Add
  case object Multiply
  case object LessThan

  trait Implicits {

    implicit val showAddNumbers: Show[Add.type] = Show.show(_ => "Add")

    implicit val addNumbersBlock: AsBlock.Aux[
      Add.type,
      Double :: Double :: HNil,
      Double :: HNil,
      false,
      _0
    ] = new AsBlock[Add.type] {
      override type IsEffect = false
      override type ExtraEffects = _0
      override type Inputs = Double :: Double :: HNil
      override type Outputs = Double :: HNil
    }

    implicit val showMultiplyNumbers: Show[Multiply.type] = Show.show(_ => "Multiply")

    implicit val multiplyNumbersBlock: AsBlock.Aux[
      Multiply.type,
      Double :: Double :: HNil,
      Double :: HNil,
      false,
      _0
    ] = new AsBlock[Multiply.type] {
      override type IsEffect = false
      override type ExtraEffects = _0
      override type Inputs = Double :: Double :: HNil
      override type Outputs = Double :: HNil
    }

    implicit val showLessThan: Show[LessThan.type] = Show.show(_ => "LessThan")

    implicit val lessThanBlock: AsBlock.Aux[
      LessThan.type,
      Double :: Double :: HNil,
      Boolean :: HNil,
      false,
      _0
    ] =
      new AsBlock[LessThan.type] {
        override type IsEffect = false
        override type ExtraEffects = _0
        override type Inputs = Double :: Double :: HNil
        override type Outputs = Boolean :: HNil
      }

  }

}

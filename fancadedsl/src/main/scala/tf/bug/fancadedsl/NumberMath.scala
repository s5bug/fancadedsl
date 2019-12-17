package tf.bug.fancadedsl

import shapeless._

object NumberMath {

  case object AddNumbers
  case object MultiplyNumbers
  case object LessThan

  trait Implicits {

    implicit val addNumbersBlock: ScriptBlock.Aux[
      AddNumbers.type,
      Double :: Double :: HNil,
      Double :: HNil,
      false,
      _0
    ] = new ScriptBlock[AddNumbers.type] {
      override type IsEffect = false
      override type ExtraEffects = _0
      override type Inputs = Double :: Double :: HNil
      override type Outputs = Double :: HNil
    }

    implicit val multiplyNumbersBlock: ScriptBlock.Aux[
      MultiplyNumbers.type,
      Double :: Double :: HNil,
      Double :: HNil,
      false,
      _0
    ] = new ScriptBlock[MultiplyNumbers.type] {
      override type IsEffect = false
      override type ExtraEffects = _0
      override type Inputs = Double :: Double :: HNil
      override type Outputs = Double :: HNil
    }

    implicit val lessThanBlock: ScriptBlock.Aux[
      LessThan.type,
      Double :: Double :: HNil,
      Boolean :: HNil,
      false,
      _0
    ] =
      new ScriptBlock[LessThan.type] {
        override type IsEffect = false
        override type ExtraEffects = _0
        override type Inputs = Double :: Double :: HNil
        override type Outputs = Boolean :: HNil
      }

  }

}

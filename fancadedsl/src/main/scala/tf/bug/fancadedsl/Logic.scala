package tf.bug.fancadedsl

import shapeless._
import shapeless.nat._

object Logic {

  case object IfThenElse

  trait Implicits {

    implicit val ifThenElseBlock
        : AsBlock.Aux[IfThenElse.type, Boolean :: HNil, HNil, true, _2] =
      new AsBlock[IfThenElse.type] {
        override type IsEffect = true
        override type ExtraEffects = _2
        override type Inputs = Boolean :: HNil
        override type Outputs = HNil
      }

  }

}

package tf.bug

import shapeless.{nat, _}
import shapeless.nat._
import shapeless.ops.hlist.{At, Length}
import tf.bug.fancadedsl.Logic.IfThenElse

package object fancadedsl
    extends DataType.Implicits
    with Rule.Implicits
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

  implicit class BlockOps[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
      block: Rule.Block[A, I, O, E, X]
  ) {

    def :=[T: DataType](vd: Variable[T])(
        implicit oneOutputEv: Length.Aux[O, _1],
        eqDt: IndicesEqual.Aux[O, _0, T :: HNil, _0, true]
    ): Rule.Block[Variable.Set[T], T :: HNil, HNil, true, _0] =
      Rule.Block[Variable.Set[T], T :: HNil, HNil, true, _0](
        Variable.Set(vd),
        (sf: Rule.Block[Variable.Set[T], T :: HNil, HNil, true, _0]) =>
          Set[Rule](Rule.Connect(block, _0, sf, _0))
      )

    def +[B, J <: HList, P <: HList, F <: Boolean, Y <: Nat](
        other: Rule.Block[B, J, P, F, Y]
    )(
        implicit meOneOutput: Length.Aux[O, _1],
        themOneOutput: Length.Aux[P, _1],
        meDouble: At.Aux[O, _0, Double],
        themDouble: At.Aux[P, _0, Double]
    ): Rule.Block[
      NumberMath.AddNumbers.type,
      Double :: Double :: HNil,
      Double :: HNil,
      false,
      _0
    ] = {
      Rule.Block[
        NumberMath.AddNumbers.type,
        Double :: Double :: HNil,
        Double :: HNil,
        false,
        _0
      ](
        NumberMath.AddNumbers,
        (additionBlock: Rule.Block[
          NumberMath.AddNumbers.type,
          Double :: Double :: HNil,
          Double :: HNil,
          false,
          _0
        ]) =>
          Set[Rule](
            Rule.Connect(block, _0, additionBlock, _0),
            Rule.Connect(other, _0, additionBlock, _1)
          )
      )
    }

    def <[B, J <: HList, P <: HList, F <: Boolean, Y <: Nat](
        other: Rule.Block[B, J, P, F, Y]
    )(
        implicit meOneOutput: Length.Aux[O, _1],
        themOneOutput: Length.Aux[P, _1],
        meDouble: At.Aux[O, _0, Double],
        themDouble: At.Aux[P, _0, Double]
    ): Rule.Block[
      NumberMath.LessThan.type,
      Double :: Double :: HNil,
      Boolean :: HNil,
      false,
      _0
    ] = {
      Rule.Block[
        NumberMath.LessThan.type,
        Double :: Double :: HNil,
        Boolean :: HNil,
        false,
        _0
      ](
        NumberMath.LessThan,
        (lessThanBlock: Rule.Block[
          NumberMath.LessThan.type,
          Double :: Double :: HNil,
          Boolean :: HNil,
          false,
          _0
        ]) =>
          Set[Rule](
            Rule.Connect(block, _0, lessThanBlock, _0),
            Rule.Connect(other, _0, lessThanBlock, _1)
          )
      )
    }

  }

  def ifB[O <: HList](
      block: Rule.Block[_, _ <: HList, O, _ <: Boolean, _ <: Nat]
  )(ifT: Option[Rule.Block[_, _ <: HList, _ <: HList, true, _ <: Nat]])(
      ifF: Option[Rule.Block[_, _ <: HList, _ <: HList, true, _ <: Nat]]
  )(
      implicit oneOutputEv: Length.Aux[O, _1],
      booleanEv: IndicesEqual.Aux[O, _0, Boolean :: HNil, _0, true]
  ): Rule.DependantBlock[
    Logic.IfThenElse.type,
    Boolean :: HNil,
    HNil,
    true,
    _2
  ] = {
    Rule.Block[Logic.IfThenElse.type, Boolean :: HNil, HNil, true, _2](
      Logic.IfThenElse,
      (ifThenElseBlock: Rule.Block[
        Logic.IfThenElse.type,
        Boolean :: HNil,
        HNil,
        true,
        _2
      ]) =>
        Set(
          ifT.map(ifTrue => Rule.On(ifThenElseBlock, _0, ifTrue)),
          ifF.map(ifFalse => Rule.On(ifThenElseBlock, _1, ifFalse)),
          Some(Rule.Connect(block, _0, ifThenElseBlock, _0))
        ).collect[Rule] { case Some(x) => x }
    )

  }

  implicit def dataAsValue[T: DataType](t: T): Value[T] = Value(t)

  implicit def valueAsBlock[T: DataType](
      v: Value[T]
  ): Rule.Block[Value.Get[T], HNil, T :: HNil, false, _0] =
    Rule.Block(Value.Get(v))

  implicit def dataAsBlock[T: DataType](
      t: T
  ): Rule.Block[Value.Get[T], HNil, T :: HNil, false, _0] =
    valueAsBlock(dataAsValue(t))

  implicit def manyRulesToOne(rules: Set[Rule]): Rule.Many = Rule.Many(rules)

}

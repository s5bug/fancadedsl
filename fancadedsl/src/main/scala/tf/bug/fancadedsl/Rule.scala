package tf.bug.fancadedsl

import shapeless.{HList, Nat}
import shapeless.ops.hlist.{At, Mapped}
import shapeless.ops.nat.LT

sealed trait Rule {

  val requirements: Set[Rule] = Set()

}

object Rule {

  sealed trait Block[A, I <: HList, O <: HList, E <: Boolean, X <: Nat]
      extends Rule {
    val block: A
  }
  case class PureBlock[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
      block: A
  )(
      implicit ev: ScriptBlock.Aux[A, I, O, E, X]
  ) extends Block[A, I, O, E, X]
  case class DependantBlock[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
      block: A,
      depends: Block[A, I, O, E, X] => Set[Rule]
  )(implicit ev: ScriptBlock.Aux[A, I, O, E, X])
      extends Block[A, I, O, E, X] {
    override val requirements: Set[Rule] = depends(this)
  }
  object Block {
    def apply[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](block: A)(
        implicit ev: ScriptBlock.Aux[A, I, O, E, X]
    ): PureBlock[A, I, O, E, X] = PureBlock(block)
    def apply[A, I <: HList, O <: HList, E <: Boolean, X <: Nat](
        block: A,
        depends: Block[A, I, O, E, X] => Set[Rule]
    )(
        implicit ev: ScriptBlock.Aux[A, I, O, E, X]
    ): DependantBlock[A, I, O, E, X] =
      DependantBlock(block, depends)
  }

  case class Connect[A, B, Ao <: HList, Bi <: HList, I <: Nat, J <: Nat](
      blockA: Block[A, _ <: HList, Ao, _ <: Boolean, _ <: Nat],
      outputIndex: I,
      blockB: Block[B, Bi, _ <: HList, _ <: Boolean, _ <: Nat],
      inputIndex: J
  )(implicit ev: IndicesEqual.Aux[Ao, I, Bi, J, true])
      extends Rule {
    override val requirements: Set[Rule] = Set(blockA, blockB)
  }
  case class Then(
      blockA: Block[_, _ <: HList, _ <: HList, true, _ <: Nat],
      blockB: Block[_, _ <: HList, _ <: HList, true, _ <: Nat]
  ) extends Rule {
    override val requirements: Set[Rule] = Set(blockA, blockB)
  }
  case class On[I <: Nat, N <: Nat](
      hub: Block[_, _ <: HList, _ <: HList, _ <: Boolean, N],
      index: I,
      next: Block[_, _ <: HList, _ <: HList, true, _ <: Nat]
  )(implicit validBranch: LT[I, N])
      extends Rule {
    override val requirements: Set[Rule] = Set(hub, next)
  }
  case class Many(rules: Set[Rule]) extends Rule {
    override val requirements: Set[Rule] = rules
  }

  trait Implicits {}

}

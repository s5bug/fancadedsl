package tf.bug.fancadedsl

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import shapeless.nat._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val fancade = Fancade[IO]
    val program: IO[(Vector[String], Vector[String])] = for {
      counter <- fancade.variable[Double]
      getCounter <- fancade.block(Variable.Get(counter))
      setCounterInc <- fancade.block(Variable.Set(counter))
      one <- fancade.value(1.0)
      oneBlock <- fancade.block(one)
      add <- fancade.block(NumberMath.Add)
      oneToAdd <- fancade.connect(oneBlock, _0, add, _0)
      counterToAdd <- fancade.connect(getCounter, _0, add, _1)
      additionToCounter <- fancade.connect(add, _0, setCounterInc, _0)
      lessThan <- fancade.block(NumberMath.LessThan)
      ifThenElse <- fancade.block(Logic.IfThenElse)
      sixty <- fancade.value(60.0)
      sixtyBlock <- fancade.block(sixty)
      counterToLessThan <- fancade.connect(getCounter, _0, lessThan, _0)
      sixtyToLessThan <- fancade.connect(sixtyBlock, _0, lessThan, _1)
      lessThanToIf <- fancade.connect(lessThan, _0, ifThenElse, _0)
      incrementIfLessThan <- fancade.on(ifThenElse, _0, setCounterInc)
      zero <- fancade.value(0.0)
      zeroBlock <- fancade.block(zero)
      setCounterZero <- fancade.block(Variable.Set(counter))
      wipeCounter <- fancade.connect(zeroBlock, _0, setCounterZero, _0)
      wipeIfSixty <- fancade.on(ifThenElse, _1, setCounterZero)
    } yield (
      Vector(getCounter.show, setCounterInc.show, oneBlock.show, add.show, lessThan.show, ifThenElse.show, sixtyBlock.show, zeroBlock.show, setCounterZero.show),
      Vector(oneToAdd.show, counterToAdd.show, additionToCounter.show, counterToLessThan.show, sixtyToLessThan.show, lessThanToIf.show, incrementIfLessThan.show, wipeCounter.show, wipeIfSixty.show)
    )
    program
      .flatMap {
        case (blocks, connections) =>
          IO(println("- Blocks:")) *>
            blocks.traverse(s => IO(println(s))) *>
            IO(println("\n- Connections:")) *>
            connections.traverse(s => IO(println(s)))
      }
      .as(ExitCode.Success)
  }

}

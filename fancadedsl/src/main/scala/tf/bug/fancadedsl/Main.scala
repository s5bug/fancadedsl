package tf.bug.fancadedsl

import cats._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import shapeless.{HNil, nat}
import shapeless.nat._

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val fancade = Fancade[IO]
    val program: IO[(Vector[String], Vector[String])] = for {
      counter <- fancade.variable[Double]
      getCounter <- fancade.block(Variable.Get(counter))
      setCounter <- fancade.block(Variable.Set(counter))
      one <- fancade.value(1.0)
      oneBlock <- fancade.block(one)
      add <- fancade.block(NumberMath.Add)
      oneToAdd <- fancade.connect(oneBlock, _0, add, _0)
      counterToAdd <- fancade.connect(getCounter, _0, add, _1)
      additionToCounter <- fancade.connect(add, _0, setCounter, _0)
    } yield (
      Vector(showBlock[Variable.Get[Double]].show(getCounter), showBlock[Variable.Set[Double]].show(setCounter), showBlock[Value[Double]].show(oneBlock), showBlock[NumberMath.Add.type].show(add)),
      Vector(showDataConnection[Double, Value[Double], NumberMath.Add.type].show(oneToAdd), showDataConnection[Double, Variable.Get[Double], NumberMath.Add.type].show(counterToAdd), showDataConnection[Double, NumberMath.Add.type, Variable.Set[Double]].show(additionToCounter))
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

package tf.bug.fancadedsl

import cats.{Applicative, FlatMap}
import cats.data.{IndexedStateT, StateT}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import shapeless.{nat, _}
import shapeless.nat._

object Main extends IOApp {

  def forLoop[F[+_]: Sync](fancade: Fancade[F], start: Int, end: Int, by: Int): IndexedStateT[F, World, World, (Fancade.Block[Logic.IfThenElse.type, Boolean :: HNil, HNil, true, Succ[nat._1]], Fancade.Block[Variable.Set[Double], Double :: HNil, HNil, true, _0], Variable[Double])] = {
    for {
      counter <- fancade.variable[Double].?
      getCounter <- fancade.block(Variable.Get(counter)).!
      setCounter <- fancade.block(Variable.Set(counter)).!
      increment <- fancade.value(by.toDouble).?
      getIncrement <- fancade.block(increment).!
      startVal <- fancade.value(start.toDouble).?
      getStart <- fancade.block(startVal).!
      endVal <- fancade.value(end.toDouble).?
      getEnd <- fancade.block(endVal).!
      lessThan <- fancade.block(NumberMath.LessThan).!
      counterIsLessThan <- fancade.connect(getCounter, _0, lessThan, _0).!
      endOfRangeConstraint <- fancade.connect(getEnd, _0, lessThan, _1).!
      ifBlock <- fancade.block(Logic.IfThenElse).!
      setCounterToStartBlock <- fancade.block(Variable.Set(counter)).!
      setCounterToStart <- fancade.connect(getStart, _0, setCounterToStartBlock, _0).!
      setCounterToStartBeforeIf <- fancade.to(setCounterToStartBlock, ifBlock).!
      lessThanToIf <- fancade.connect(lessThan, _0, ifBlock, _0).!
      addBlock <- fancade.block(NumberMath.Add).!
      getIncrementToAdd <- fancade.connect(getIncrement, _0, addBlock, _0).!
      getCounterToAdd <- fancade.connect(getCounter, _0, addBlock, _1).!
      addResultToSetCounter <- fancade.connect(addBlock, _0, setCounter, _0).!
      setCounterToIf <- fancade.to(setCounter, ifBlock).!
    } yield (ifBlock, setCounter, counter)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val fancade = Fancade[IO]
    val program: Program[IO] = for {
      a <- fancade.variable[Double].?
      getA <- fancade.block(Variable.Get(a)).!
      b <- fancade.variable[Double].?
      getB <- fancade.block(Variable.Get(b)).!
      c <- fancade.variable[Double].?
      getC <- fancade.block(Variable.Get(c)).!
      (entry, exit, _) <- forLoop(fancade, 0, 10, 1)
      zero <- fancade.value(0.0).?
      zeroBlock <- fancade.block(zero).!
      one <- fancade.value(1.0).?
      oneBlock <- fancade.block(one).!
      setAToZeroBlock <- fancade.block(Variable.Set(a)).!
      setAToZero <- fancade.connect(zeroBlock, _0, setAToZeroBlock, _0).!
      setBToOneBlock <- fancade.block(Variable.Set(b)).!
      setBToOne <- fancade.connect(oneBlock, _0, setBToOneBlock, _0).!
      _ <- fancade.to(setAToZeroBlock, setBToOneBlock).!
      enterLoop <- fancade.to(setBToOneBlock, entry).!
      addAAndBBlock <- fancade.block(NumberMath.Add).!
      addA <- fancade.connect(getA, _0, addAAndBBlock, _0).!
      addB <- fancade.connect(getB, _0, addAAndBBlock, _1).!
      storeResultInCBlock <- fancade.block(Variable.Set(c)).!
      storeResultInC <- fancade.connect(addAAndBBlock, _0, storeResultInCBlock, _0).!
      _ <- fancade.on(entry, _0, storeResultInCBlock).!
      copyBToABlock <- fancade.block(Variable.Set(a)).!
      copyBToA <- fancade.connect(getB, _0, copyBToABlock, _0).!
      _ <- fancade.to(storeResultInCBlock, copyBToABlock).!
      copyCToBBlock <- fancade.block(Variable.Set(b)).!
      copyCToB <- fancade.connect(getC, _0, copyCToBBlock, _0).!
      _ <- fancade.to(copyBToABlock, copyCToBBlock).!
      exitLoop <- fancade.to(copyCToBBlock, exit).!
    } yield ()
    program.runEmptyS.flatMap(w => IO(println(w.show))).as(ExitCode.Success)
  }

}

package tf.bug.fancadedsl

import cats._
import cats.implicits._
import shapeless.{HNil, nat}
import shapeless.nat._

object Main {

  val program: Rule = {
    val counter: Variable[Double] = Variable[Double]("Counter")
    val getCounter = Rule.Block(Variable.Get(counter))

    ifB(getCounter < 10.0)(Some((getCounter + 1.0) := counter))(None)
  }

  def main(args: Array[String]): Unit = {
    lazy val rulesFlattening: LazyList[Set[Rule]] =
      (program.requirements + program) #:: rulesFlattening.map(
        _.flatMap(r => r.requirements + r)
      )
    val allFlattens =
      rulesFlattening.takeWhile(s => s.flatMap(r => r.requirements + r) != s)
    println(allFlattens.last.mkString("\n"))
  }

}

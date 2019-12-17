package tf.bug.fancadedsl

sealed trait DataType[T] {

  val name: String

}

object DataType {

  trait Implicits {

    implicit val numberDataType: DataType[Double] = new DataType[Double] {
      override val name: String = "number"
    }

  }

}

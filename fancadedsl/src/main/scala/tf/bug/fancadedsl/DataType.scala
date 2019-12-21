package tf.bug.fancadedsl

sealed trait DataType[T] {

  val name: String

}

object DataType {

  trait Implicits {

    implicit def quotedDataType[T](
        implicit inner: DataType[T]
    ): DataType[Quoted[T]] = new DataType[Quoted[T]] {
      override val name: String = s"'${inner.name}"
    }

    implicit val numberDataType: DataType[Double] = new DataType[Double] {
      override val name: String = "number"
    }

    implicit val truthDataType: DataType[Boolean] = new DataType[Boolean] {
      override val name: String = "truth"
    }

  }

}

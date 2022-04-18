package tf.bug.fancadedsl.polymorphic

trait Instance[F[_]] {

  type A
  val first: A
  val second: F[first.type]

}

object Instance {

  case class PartialApplyOps[F[_]]() {
    def apply[A](a: A)(implicit fa: F[a.type]): Instance[F] = new Instance[F] {
      override type A = a.type
      override val first: a.type = a
      override val second: F[first.type] = fa
    }
  }

  def apply[F[_]]: PartialApplyOps[F] = PartialApplyOps[F]()

}

package tf.bug.fancadedsl.polymorphic

trait Sigma[+A, F[_]] {

  val first: A
  val second: F[first.type]

}

object Sigma {

  def apply[A, F[_]](a: A)(fa: F[a.type]): Sigma[A, F] = new Sigma[A, F] {
    override val first: a.type = a
    override val second: F[first.type] = fa
  }

}

package tf.bug

import cats.Show
import cats.data.StateT
import tf.bug.fancadedsl.polymorphic.Sigma

package object fancadedsl
    extends Fancade.Implicits
    with DataType.Implicits
    with Variable.Implicits
    with Value.Implicits
    with NumberMath.Implicits
    with Logic.Implicits
    with Connectable.Implicits
    with BlocksConnectable.Implicits
    with World.Implicits {

  type Quoted[+T] <: T

  type Program[F[_]] = StateT[F, World, Unit]

  implicit def sigmaFromShow[A](a: A)(implicit show: Show.ContravariantShow[A]): Sigma[A, Show.ContravariantShow] =
    Sigma.apply[A, Show.ContravariantShow](a)(show)

}

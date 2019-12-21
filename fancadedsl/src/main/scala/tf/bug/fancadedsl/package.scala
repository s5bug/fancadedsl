package tf.bug

import cats.Show
import polymorphic.Sigma

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

  implicit def sigmaFromShow[A](a: A)(implicit show: Show.ContravariantShow[A]): Sigma[A, Show.ContravariantShow] = Sigma.apply[A, Show.ContravariantShow](a)(show)

}

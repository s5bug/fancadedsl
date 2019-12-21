package tf.bug

package object fancadedsl
    extends Fancade.Implicits
    with DataType.Implicits
    with Variable.Implicits
    with Value.Implicits
    with NumberMath.Implicits
    with Logic.Implicits
    with Connectable.Implicits
    with BlocksConnectable.Implicits {

  type Quoted[+T] <: T

}

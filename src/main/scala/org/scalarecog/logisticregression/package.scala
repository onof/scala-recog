package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 01/08/11
 */

import scala.math._
package object logisticregression {

  def sigmoid(x : Double) =
    1.0 / (1.0 + exp(-x))

}

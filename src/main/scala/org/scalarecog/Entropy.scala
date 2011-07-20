package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 20/07/11
 */

import scala.math._

/*
 * Shannon Entropy
 */
object Entropy {

  def apply[Data,Property](dataset : List[Data])(implicit getProperty : Data => Property) : Double =
    dataset.groupBy(getProperty).values
      .map(i => i.length / dataset.length)
      .map(p => - p * log10(p) / log10(2)).sum
}

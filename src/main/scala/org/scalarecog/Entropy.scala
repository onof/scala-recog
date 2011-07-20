package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 20/07/11
 */

import scala.math._

class Entropy[Data,Property](getProperty : Data => Property) {

  def apply(dataset : List[Data]) : Double =
    dataset.groupBy(getProperty).values
      .map(i => i.length.toDouble / dataset.length)
      .map(p => - p * log10(p) / log10(2))
      .sum
}

/*
 * Shannon Entropy
 */
object Entropy {

  def apply[Data](dataset : List[Data]) : Double =
    new Entropy[Data, Data](identity[Data])(dataset)
}


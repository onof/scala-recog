package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 20/07/11
 */

import scala.math._

class Entropy[Data](dataset : List[Data]) {

  def entropy : Double = entropy[Data](identity[Data])

  def entropy[Property](getProperty : Data => Property) : Double =
    dataset.groupBy(getProperty).values
      .map(i => i.length.toDouble / dataset.length)
      .map(p => - p * log10(p) / log10(2))
      .sum
}

/*
 * Shannon Entropy
 */
object Entropy {

  implicit def traversable2Entropy[Property](x : Traversable[Property]) =
    new Entropy[Property](x.toList);
}


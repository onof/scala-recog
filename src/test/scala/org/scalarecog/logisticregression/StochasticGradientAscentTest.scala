package org.scalarecog.logisticregression

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.math._;
import scalala.tensor.::
import scalala.tensor.mutable._;
import scalala.tensor.dense._
;

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 04/08/11
 */

class StochasticGradientAscentTest extends FlatSpec with ShouldMatchers {

  "SigmoidClassifier " should " classify in a well splitted dataset " in {

    val trainingSet = DenseMatrix.tabulate(300, 3)((i, j) => j match {
      case 0 => 1.0
      case 1 => random * 20.0 + 10.0
      case 2 => (random * 20.0 + 10.0) * (if (i > 150) -1.0 else 1.0)
    })

    val labels = DenseVectorCol.tabulate(trainingSet.numRows)(i => trainingSet(i, 2) > 0.0)

    val ts = for ( i <- 0 until trainingSet.numRows ) yield (trainingSet(i, ::), labels(i))

    val trainer = new StochasticGradientAscentTrainer(ts)
    val classifier = trainer train

    classifier.classify(Vector(1.0, 13, 14)) should equal(true)
    classifier.classify(Vector(1.0, 22, -22)) should equal(false)

    for (k <- 0 until trainingSet.numRows) {
      val vTest = trainingSet(k, ::)
      val bRet = labels(k)
      classifier.classify(vTest) should equal (bRet)
    }
  }
}
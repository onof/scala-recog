package org.scalarecog.logisticregression

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.math._;
import scalala.tensor.::
import scalala.tensor.mutable._;
import scalala.tensor.dense._;

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 01/08/11
 */

/**
 *
 */
class SigmoidTest extends FlatSpec with ShouldMatchers {

  "Sigmoid " should " be 0.5 if applied to 0" in {
    sigmoid(0) should equal (0.5)
  }

  it should " be between 0 and 1" in {
    for (
      k <- 1 until 2500;
      kd = k*100000.0
    )
    {
      sigmoid( kd) should { be <= (1.0) and be >= (0.5) }
      sigmoid(-kd) should { be <= (0.5) and be >= (0.0) }
    }
  }
}

/**
 *
 */
class GradientAscentTest extends FlatSpec with ShouldMatchers {

  "SigmoidClassifier " should " classify in a well splitted dataset " in {

    val trainingSet = DenseMatrix.tabulate(300, 3)((i, j) => j match {
      case 0 => 1.0
      case 1 => random * 20.0 + 10.0
      case 2 => (random * 20.0 + 10.0) * (if (i > 150) -1.0 else 1.0)
    })

    val labels = DenseVectorCol.tabulate(trainingSet.numRows)(i => trainingSet(i, 2) > 0.0)

    val trainer = new GradientAscentSigmoidTrainer(trainingSet, labels, 500, 0.001)
    val classifier = trainer.asSigmoidTrainer

    classifier.classify(Vector(1.0, 13, 14)) should equal(true)
    classifier.classify(Vector(1.0, 22, -22)) should equal(false)

    for (k <- 0 until trainingSet.numRows) {
      val vTest = trainingSet(k, ::)
      val bRet = labels(k)
      classifier.classify(vTest) should equal (bRet)
    }
  }
}

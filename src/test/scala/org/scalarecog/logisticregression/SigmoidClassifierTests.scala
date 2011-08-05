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
class SigmoidClassifierTests extends FlatSpec with ShouldMatchers {

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

  "SigmoidClassifier with GradientAscent trainer " should " classify in a well splitted dataset " in {

    val ts = DenseMatrix.tabulate(300, 3)((i, j) => j match {
      case 0 => 1.0
      case 1 => random * 20.0 + 10.0
      case 2 => (random * 20.0 + 10.0) * (if (i > 150) -1.0 else 1.0)
    })

    val classLabels = DenseVectorCol.tabulate(ts.numRows)(i => ts(i, 2) > 0.0)

    val trainer = new SigmoidTrainer with GradientAscent with FixedIterations {
      val trainingSet = ts
      val labels = classLabels.map(if(_) 1.0 else 0.0)
    }

    val classifier = trainer train

    classifier.classify(Vector(1.0, 13, 14)) should equal(true)
    classifier.classify(Vector(1.0, 22, -22)) should equal(false)

    for (k <- 0 until ts.numRows) {
      val vTest = ts(k, ::)
      val bRet = classLabels(k)
      classifier.classify(vTest) should equal (bRet)
    }
  }

  it should " be customizable " in {

    val trainer1 = new SigmoidTrainer with GradientAscent with FixedIterations {
      val trainingSet = DenseMatrix.zeros[Double](50, 3)
      val labels = DenseVectorCol.zeros[Double](50)
      override val numIterations : Int = 24
    }

    val trainer2 = new SigmoidTrainer with GradientAscent with FixedIterations {
      val trainingSet = DenseMatrix.zeros[Double](50, 3)
      val labels = DenseVectorCol.zeros[Double](50)
      override val alpha = 0.1
    }
  }

   "SigmoidClassifier " should " create a classifier with implicit conversion " in {

      val ts = for ( i <- 0 until 300 )
              yield (DenseVector.tabulate(2)(i => random*10.0).t, random > 0.5)

      import org.scalarecog.logisticregression.SigmoidClassifier._
      val classifier = ts.train

      classifier should not equal (null)
  }


  "SigmoidClassifier with Stochastic trainer " should " classify in a well splitted dataset " in {

    val trainingItems = DenseMatrix.tabulate(300, 3)((i, j) => j match {
      case 0 => 1.0
      case 1 => random * 20.0 + 10.0
      case 2 => (random * 20.0 + 10.0) * (if (i > 150) -1.0 else 1.0)
    })

    val labels = DenseVectorCol.tabulate(trainingItems.numRows)(i => trainingItems(i, 2) > 0.0)

    val ts = for ( i <- 0 until trainingItems.numRows ) yield (trainingItems(i, ::), labels(i))

    val trainer =  new SigmoidTrainer with ImprovedStochasticGradientAscent with FixedIterations {
      val trainingSet = ts
    }

    // new StochasticGradientAscentTrainer(ts)
    val classifier = trainer train

    classifier.classify(Vector(1.0, 13, 14)) should equal(true)
    classifier.classify(Vector(1.0, 22, -22)) should equal(false)

    for (k <- 0 until trainingItems.numRows) {
      val vTest = trainingItems(k, ::)
      val bRet = labels(k)
      classifier.classify(vTest) should equal (bRet)
    }
  }

  it should " be customizable " in {

    val ts = for ( i <- 0 until 300 )
              yield (DenseVector.tabulate(2)(i => random*10.0).t, random > 0.5)

    val trainer1 = new SigmoidTrainer with ImprovedStochasticGradientAscent with FixedIterations {
      val trainingSet = ts
      override val numIterations = 1000
    }

    val trainer2 = new SigmoidTrainer with StochasticGradientAscent with FixedIterations {
      val trainingSet = ts

      override def alpha(i : Int) : Double = 2/(1.0+i )+0.01
    }
  }

  "SigmoidTrainer " should " be customizable " in {

    val ts = for ( i <- 0 until 32 )
              yield (DenseVector.tabulate(2)(i => random*10.0).t, random > 0.5)

    val trainer1 = new SigmoidTrainer with FixedIterations {
      val startWeights = DenseVectorCol.ones[Double](32)
      val trainingSet = ts

      def improveWeights(weights : VectorCol[Double]) = weights /* ... more code ... */

    }

    trait MyStopCondition extends Trainer[Double] {
      lazy val weights = {
         var w = startWeights
         while(true)
          w = improveWeights(w)
         w
      }
    }

    val trainer2 = new SigmoidTrainer with StochasticGradientAscent with MyStopCondition {
      val trainingSet = ts
    }
  }
}

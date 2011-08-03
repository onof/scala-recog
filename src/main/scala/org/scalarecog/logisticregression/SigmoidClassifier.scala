package org.scalarecog.logisticregression

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 01/08/11
 */

import org.scalarecog.Classifier

import scalala.scalar._;

import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._
import scalala.tensor.{LiteralRow, ::}
;

class SigmoidClassifier(val weights: VectorCol[Double])
  extends Classifier[Vector[Double], Boolean]
{
  def classify(v : Vector[Double]) = if (v.length != weights.length) throw new Exception("Invalid argument")
    else sigmoid(v.asRow * weights) > 0.5
}

class GradientAscentSigmoidTrainer
(
  val trainingSet: Matrix[Double],
  val labels: VectorCol[Boolean],
  val numIterations : Int,
  val alpha : Double
)
{
  private lazy val weights = {
    val labelVect = labels.map(if(_) 1.0 else 0.0)

    var w = DenseVectorCol.ones[Double](trainingSet.numCols)
    for (k <- 0 to numIterations) {
      val error = labelVect - ((trainingSet * w) map sigmoid)
      w = w + (trainingSet.t * error) * alpha
    }
    w
  }

  lazy val asSigmoidTrainer = new SigmoidClassifier(weights)
}

object SigmoidClassifier {

  def trainWith[Label](trainingSet: List[(VectorRow[Double], Label)]) = null

}

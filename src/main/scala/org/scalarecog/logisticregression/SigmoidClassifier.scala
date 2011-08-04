package org.scalarecog.logisticregression

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 01/08/11
 */

import org.scalarecog.Classifier


import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._
import scalala.tensor.{LiteralRow, ::}
import scalala.operators.{MatrixOps, NumericOps}


class SigmoidClassifier(val weights: VectorCol[Double])
  extends Classifier[Vector[Double], Boolean]
{
  def classify(v : Vector[Double]) = if (v.length != weights.length) throw new Exception("Invalid argument")
    else sigmoid(v.asRow * weights) > 0.5
}

trait Trainer[Data] {
  val startWeights: VectorCol[Data]
  def improveWeights(weights : VectorCol[Data]) : VectorCol[Data]
}

trait FixedIterationsTrainer extends Trainer[Double] {
  val numIterations : Int

  lazy val weights = {
    var w = startWeights
    for (k <- 0 to numIterations) {
      w = improveWeights(w)
    }
    w
  }
}

trait GradientAscent extends Trainer[Double] {
  type Data = Double

  val alpha : Data
  val trainingSet: Matrix[Data]
  val labels: VectorCol[Data]
  val function : Function[VectorCol[Data], VectorCol[Data]]

  val startWeights: VectorCol[Data] = DenseVectorCol.ones[Data](trainingSet.numCols)

  def improveWeights(weights : VectorCol[Data]) = {
    val error = labels - function((trainingSet * weights))
    weights + (trainingSet.t * error) * alpha
  }
}

trait SigmoidTrainer {
  val weights : VectorCol[Double]

  def train = new SigmoidClassifier(weights)
}

class GradientAscentSigmoidTrainer (
  val trainingSet: Matrix[Double],
  val label1: VectorCol[Boolean],
  val numIterations : Int = 500,
  val alpha : Double = 0.001
)
extends GradientAscent with SigmoidTrainer with FixedIterationsTrainer
{
  val function : Function[VectorCol[Double], VectorCol[Double]] = v => v map sigmoid

  val labels = label1.map(if(_) 1.0 else 0.0)
}

trait StochasticGradientAscent extends Trainer[Double] {

  val trainingSet : IndexedSeq[(VectorRow[Double], Boolean)]
  def alpha(i : Int) : Double
  val function : Function[Double, Double]

  val startWeights: VectorCol[Double] = DenseVectorCol.ones[Double](trainingSet.head._1.length)

  def improveWeights(weights : VectorCol[Double]) = {

    var dataIndex = (0 until trainingSet.length).toIndexedSeq
    var w = weights
    for ( i <- 0 until trainingSet.length) {
      val indexOfIndex = (scala.math.random * (dataIndex.size - 1)).toInt
      val index = dataIndex(indexOfIndex)
      val h = function( trainingSet(index)._1 * weights )
      val error = (if(trainingSet(index)._2) 1.0 else 0.0) - h

      w = w + trainingSet(index)._1 * (alpha(i) * error)
      dataIndex = dataIndex.filterNot(_ == index)
    }
    w
  }
}

class StochasticGradientAscentTrainer
(
  val trainingSet : IndexedSeq[(VectorRow[Double], Boolean)],
  val numIterations : Int = 500
)
extends StochasticGradientAscent with SigmoidTrainer with FixedIterationsTrainer
{
  private var j = 0
  def alpha(i : Int) = 4/(1.0+j+i)+0.01

  val function : Function[Double, Double] = sigmoid

  override def improveWeights(weights : VectorCol[Double]) = {
    val w = super.improveWeights(weights)
    j = j + 1
    w
  }
}

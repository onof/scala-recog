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

trait FixedIterations extends Trainer[Double] {
  val numIterations : Int = 500

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

  val alpha : Data = 0.001
  val trainingSet: Matrix[Data]
  val labels: VectorCol[Data]
  val function : Function[Data, Data]

  lazy val startWeights: VectorCol[Data] = DenseVectorCol.ones[Data](trainingSet.numCols)

  def improveWeights(weights : VectorCol[Data]) = {
    val error = labels - ((trainingSet * weights) map function)
    weights + (trainingSet.t * error) * alpha
  }
}

trait SigmoidTrainer {
  val weights : VectorCol[Double]

  val function : Function[Double, Double] = sigmoid

  def train = new SigmoidClassifier(weights)
}

trait StochasticGradientAscent extends Trainer[Double] {

  val trainingSet : IndexedSeq[(VectorRow[Double], Boolean)]

  def alpha(i : Int) : Double = 0.001

  val function : Function[Double, Double]

  lazy val startWeights: VectorCol[Double] = DenseVectorCol.ones[Double](trainingSet.head._1.length)

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

trait ImprovedStochasticGradientAscent extends StochasticGradientAscent {

  protected var j : Int = 0
  def fixedAlpha(j : Int)(i : Int) = 4/(1.0+j+i)+0.01

  abstract override def alpha(i : Int) = fixedAlpha(j)(j)

  abstract override def improveWeights(weights : VectorCol[Double]) = {
    val w = super.improveWeights(weights)
    j = j + 1
    w
  }
}

class StochasticGradientAscentTrainer
(
  val trainingSet : IndexedSeq[(VectorRow[Double], Boolean)],
  override val numIterations : Int = 500
)
extends StochasticGradientAscent with SigmoidTrainer with FixedIterations
{
  private var j = 0
  override def alpha(i : Int) = 4/(1.0+j+i)+0.01

  override def improveWeights(weights : VectorCol[Double]) = {
    val w = super.improveWeights(weights)
    j = j + 1
    w
  }
}

/**
 * Provides implicit conversion for default values
 */
object SigmoidClassifier {

  implicit def toSigmoidClassifierTrainer(ts: IndexedSeq[(VectorRow[Double], Boolean)]) =
    new SigmoidTrainer with ImprovedStochasticGradientAscent with FixedIterations {
      val trainingSet = ts
  }
}

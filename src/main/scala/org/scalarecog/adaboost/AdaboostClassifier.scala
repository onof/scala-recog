package org.scalarecog.adaboost

import org.scalarecog.{SupervisedLearning, Classifier}
import scala.math._
import scala.collection.mutable.ListBuffer

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 21/09/11
 */
class AdaboostClassifier[Data, Label](classifiers : List[(Classifier[Data, Label], Double)])
    extends Classifier[Data, Label]
  {
     // take the classification with the best rate
     def classify(d : Data) = {
       val ht = for (
          (h, alpha) <- classifiers;
          l = h.classify(d)
        ) yield (l, alpha)

       val grouped = ht.groupBy(h => h._1).mapValues(v => v.map(t => t._2).sum)
       grouped.maxBy(_._2)._1
     }
  }

trait AdaboostTrainer[Data, Label] extends SupervisedLearning[Data, Label, AdaboostClassifier[Data, Label]] {

  val numSteps : Int

  def getError(errorList : Traversable[Double]) : Double

  def getAlpha(error : Double) : Double = log((1.0-error)/error)/2.0

  def updateDistribution(distribution : Traversable[Double], errorList : List[Double]) : List[Double]

  /**
   *
   */
  def getClassifier(ts : List[(Data, Label)])(distribution : List[Double]) : (Classifier[Data, Label], List[Double])

  def trainWith(trainingSet : Traversable[(Data, Label)]) = {

    val ts = trainingSet.toList
    var distribution = List.fill(ts.length)(1.0 / ts.length)

    val a = ListBuffer[(Classifier[Data, Label], Double)]()

    for (
        i <- 1 to numSteps;
        (c, err) = getClassifier(ts)(distribution)
    )
    {
      val error = getError(err);
      val alpha = getAlpha(error);
      distribution = updateDistribution(distribution, err);

      a += ((c, alpha))
    }

    new AdaboostClassifier[Data, Label](a toList)
  }
}



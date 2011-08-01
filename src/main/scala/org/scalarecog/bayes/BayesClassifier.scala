package org.scalarecog.bayes

import org.scalarecog.Classifier

/**
 * Bayes classifier.
 *
 * User: onofrio.panzarino@gmail.com
 * Date: 25/07/11
 */
class BayesClassifier[DataItem,Label](
                                      dataSet : List[(List[DataItem],Label)]
                                      )
  extends Classifier[List[DataItem],Label] {

  type Data = List[DataItem]

  private val labels = dataSet groupBy(_._2) keys

  private val items = dataSet.flatMap(_._1).toSet.size

  // to compute probability of every class
  private val labelsFrequency = dataSet groupBy(_._2) mapValues (_.length)
  private val dataSetLength = dataSet.length

  private val labelsPerItemFrequency : Map[Label, Int] =
    (for (
        (data, label) <- dataSet;
        item <- data
      ) yield (item, label)
    ) groupBy (_._2) mapValues(_.length)

  // to compute probability of every item
  private val itemFrequency : Map[DataItem, Map[Label, Int]] =
    (for (
        (data, label) <- dataSet;
        item <- data
      ) yield (item, label)
    ) groupBy (_._1) mapValues(_.groupBy (_._2) mapValues (_.length))

  private def conditionalProbability(label : Label, data : DataItem) : Double = {
    val pMC = itemFrequency.getOrElse(data, Map[Label,Int]()).get(label) match {
      case Some(a : Int) => a.toDouble / labelsPerItemFrequency.getOrElse(label, 1)
      case None => 0.0001
    }

    val pC = (labelsFrequency.getOrElse(label, 0).toDouble / dataSetLength)
    val pM = (itemFrequency.getOrElse(data, Map[Label,Int](label -> 1)).values.sum.toDouble / items)

    pMC * pC / pM
  }

  private def conditionalProbability(label : Label, data : Data) : Double = {
    val itemProbabilities = (data map (conditionalProbability(label, _)))
    itemProbabilities.foldLeft(1.0)(_ * _)
  }

  def classify(data : Data) : Label = {
    val labelProbabilities = labels.map(l =>(l, conditionalProbability(l, data)))
    labelProbabilities.reduceLeft((t1, t2) => if(t1._2 > t2._2) t1 else t2)._1
  }
}

/**
 * Bayes classifier companion objects.
 * It contains implicit conversions
 */
object BayesClassifier {

  class BayesClassifierM[DataItem,Label](l : Traversable[(List[DataItem],Label)]) {
    def asBayesTrainingSet = new BayesClassifier[DataItem,Label](l.toList)
  }

  implicit def list2Bayes[DataItem,Label](l : Traversable[(List[DataItem],Label)]) =
    new BayesClassifierM[DataItem,Label](l)

}

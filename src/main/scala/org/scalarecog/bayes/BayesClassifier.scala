package org.scalarecog.bayes

import org.scalarecog.{SupervisedLearning, Classifier}

/**
 * Bayes classifier trainer. It computes conditional probabilities needed by the classifier.
 */
class BayesTrainer[DataItem,Label] extends SupervisedLearning[List[DataItem], Label, BayesClassifier[DataItem, Label]] {

  def trainWith(trainingSet: Traversable[(List[DataItem], Label)]) : BayesClassifier[DataItem,Label] = {
    type Data = List[DataItem]

    val dataSet = trainingSet.toList
    val labels = ( dataSet groupBy(_._2) keys ) toList

    val items = dataSet.flatMap(_._1).toSet.size

    val labelsFrequency = dataSet groupBy(_._2) mapValues (_.length)
    val dataSetLength = dataSet.length


    val labelsPerItemFrequency : Map[Label, Int] =
    (for (
        (data, label) <- dataSet;
        item <- data
      ) yield (item, label)
    ) groupBy (_._2) mapValues(_.length)

    // to compute probability of every item
    val itemFrequency : Map[DataItem, Map[Label, Int]] =
      (for (
        (data, label) <- dataSet;
        item <- data
        ) yield (item, label)
      ) groupBy (_._1) mapValues(_.groupBy (_._2) mapValues (_.length))

    def conditionalProbability(label : Label, data : DataItem) : Double = {
      val pMC = itemFrequency.getOrElse(data, Map[Label,Int]()).get(label) match {
        case Some(a : Int) => a.toDouble / labelsPerItemFrequency.getOrElse(label, 1)
        case None => 0.0001
      }

      val pC = (labelsFrequency.getOrElse(label, 0).toDouble / dataSetLength)
      val pM = (itemFrequency.getOrElse(data, Map[Label,Int](label -> 1)).values.sum.toDouble / items)

      pMC * pC / pM
    }

    val conditionalProbabilities =
    (for (
      item : DataItem <- dataSet.flatMap(_._1).toSet;
      label <- labels;
      cp = conditionalProbability(label, item)
      ) yield ((item, label), cp)
    ).toMap


    new BayesClassifier[DataItem, Label](labels, conditionalProbabilities)
  }

}

/**
 * Bayes classifier.
 *
 * User: onofrio.panzarino@gmail.com
 * Date: 25/07/11
 */
class BayesClassifier[DataItem,Label](
                                      val labels : List[Label],
                                      val conditionalProbabilities : Map[(DataItem, Label), Double]
                                      )
  extends Classifier[List[DataItem],Label] {

  type Data = List[DataItem]

  private def conditionalProbability(label : Label, data : Data) : Double = {
    val itemProbabilities = data map ( di => conditionalProbabilities((di, label)))
    itemProbabilities.foldLeft(1.0)(_ * _)
  }

  def classify(data : Data) : Label = {
    val labelProbabilities = labels.map(l =>(l, conditionalProbability(l, data)))
    labelProbabilities.reduceLeft((t1, t2) => if(t1._2 > t2._2) t1 else t2)._1
  }

  // def trainWith(data : List[DataItem], label : Label) = new BayesTrainer[DataItem,Label].trainWith((data, label) :: dataSet)
}

/**
 * Bayes classifier companion objects.
 * It contains implicit conversions
 */
object BayesClassifier {

  class BayesClassifierM[DataItem,Label](l : Traversable[(List[DataItem],Label)]) {
    def asBayesTrainingSet = new BayesTrainer[DataItem, Label].trainWith(l)
  }

  implicit def list2Bayes[DataItem,Label](l : Traversable[(List[DataItem],Label)]) =
    new BayesClassifierM[DataItem,Label](l)
}

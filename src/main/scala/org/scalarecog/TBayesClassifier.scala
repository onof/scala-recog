package org.scalarecog.bayes

import org.scalarecog.{ SupervisedLearning, Classifier }

/**
 * Bayes classifier trainer. It computes conditional probabilities needed by the classifier.
 */
class TBayesTrainer[DataItem, Label] extends SupervisedLearning[List[(DataItem, Double)], Label, TBayesClassifier[DataItem, Label]] {

//  def trainWith(trainingSet: Traversable[(List[DataItem], Label)]): BayesClassifier[DataItem, Label] = {
//    val freqProcessedDataSet = trainingSet.map{ documentData =>
//      (documentData._1.groupBy(dataItem=>dataItem).map(groupedData=>(groupedData._1,groupedData._2.length.toDouble)).toList, documentData._2)
//    }
//    trainWith(freqProcessedDataSet)
//  }
  
  def trainWith(trainingSet: Traversable[(List[(DataItem, Double)], Label)]): TBayesClassifier[DataItem, Label] = {
    type Data = List[(DataItem, Double)]

    val dataSet = trainingSet.toList
    val labels = (dataSet groupBy (_._2) keys) toList

    val items = dataSet.flatMap(_._1).toSet.size

    val labelsFrequency = dataSet groupBy (_._2) mapValues (_.length)
    val dataSetLength = dataSet.length
    val labelDataItemCombos = (for (
      (data, label) <- dataSet;
      item <- data
    ) yield (item, label))
    
    val labelsPerItemFrequency: Map[Label, Double] =
     labelDataItemCombos groupBy (_._2) mapValues (_.map(_._1._2).sum)



    // to compute probability of every item
    val itemFrequency: Map[DataItem, Map[Label, Double]] =
      labelDataItemCombos groupBy (_._1._1) mapValues (_.groupBy(_._2) mapValues (_.map(_._1._2).sum))

    def conditionalProbability(label: Label, data: DataItem, a:Double = 0.0001): Double = {
      val pMC = itemFrequency.getOrElse(data, Map[Label, Double]()).get(label) match {
        case Some(a: Double) => a / labelsPerItemFrequency.getOrElse(label, 1.0)
        case None => a
      }

      val pC = (labelsFrequency.getOrElse(label, 0).toDouble / dataSetLength)
      val pM = (itemFrequency.getOrElse(data, Map[Label, Double](label -> 1)).values.sum.toDouble / items)

      pMC * pC / pM
    }

    val conditionalProbabilities =
      (for (
        item: (DataItem, Double) <- dataSet.flatMap(_._1).toSet;
        label <- labels;
        cp = conditionalProbability(label, item._1) * item._2
      ) yield ((item._1, label), cp)).toMap

    new TBayesClassifier[DataItem, Label](labels, conditionalProbabilities)
  }

}

/**
 * Bayes classifier.
 *
 * User: onofrio.panzarino@gmail.com
 * Date: 25/07/11
 */
class TBayesClassifier[DataItem, Label](
  val labels: List[Label],
  val conditionalProbabilities: Map[(DataItem, Label), Double])
  extends Classifier[List[(DataItem, Double)], Label] {

  type Data = List[(DataItem, Double)]

  private def conditionalProbability(label: Label, data: Data): Double = {
    val itemProbabilities = data map (di => di._2 * conditionalProbabilities((di._1, label)))
    itemProbabilities.foldLeft(1.0)(_ * _)
  }

  def classify(data: Data): Label = {
    val labelProbabilities = labels.map(l => (l, conditionalProbability(l, data)))
    labelProbabilities.reduceLeft((t1, t2) => if (t1._2 > t2._2) t1 else t2)._1
  }
  def proportions(data:Data) = {
        val labelProbabilities = labels.map(l => (l, conditionalProbability(l, data)))
    labelProbabilities//.reduceLeft((t1, t2) => if (t1._2 > t2._2) t1 else t2)._1
    
  }
  // def trainWith(data : List[DataItem], label : Label) = new BayesTrainer[DataItem,Label].trainWith((data, label) :: dataSet)
}

/**
 * Bayes classifier companion objects.
 * It contains implicit conversions
 */
object TBayesClassifier {

  class TBayesClassifierM[DataItem, Label](l: Traversable[(List[(DataItem, Double)], Label)]) {
    def asBayesTrainingSet = new TBayesTrainer[DataItem, Label].trainWith(l)
  }

  implicit def list2Bayes[DataItem, Label](l: Traversable[(List[(DataItem, Double)], Label)]) =
    new TBayesClassifierM[DataItem, Label](l)
    
//    val test = List((List(("test",1.0), ("test2",2.0)),"test"))
//    test.asBayesTrainingSet
    
}

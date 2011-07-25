package org.scalarecog.bayes

import org.scalarecog.{FuzzyClassifier, Classifier}
import javax.swing.ViewportLayout
import java.awt.Label

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 25/07/11
 */

class BayesClassifier[DataItem,Label](
                                      dataSet : List[(List[DataItem],Label)]
                                      )
  extends FuzzyClassifier[List[DataItem],Label] {

  type Data = List[DataItem]

  private val labels = dataSet groupBy(_._2) keys

  private val items = dataSet.flatMap(_._1).length

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

  private def conditionalProbability(label : Label, data : DataItem) : Double =
    (itemFrequency.getOrElse(data, Map[Label,Int]()).getOrElse(label, 0).toDouble /
     labelsPerItemFrequency.getOrElse(label, 0)) *
    (labelsFrequency.getOrElse(label, 0).toDouble / dataSetLength) /
    (itemFrequency(data).values.sum.toDouble / items)


  private def conditionalProbability(label : Label, data : Data) : Double =
    (data map (conditionalProbability(label, _))).foldLeft(0.0)(_ * _)

  def classifyFuzzy(data : Data) : (Label, Double) =
    labels.map(l =>(l, conditionalProbability(l, data))).reduceLeft((t1, t2) => if(t1._2 > t2._2) t1 else t2)
}

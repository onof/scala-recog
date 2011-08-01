package org.scalarecog.decisiontree

import org.scalarecog.Entropy._

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 21/07/11
 */

/**
 * A subclass of DecisionBranch for vectors
 */
class DecisionBranchVector[Label, Property](
                                             val index: Int,
                                             branches: Map[Property, DecisionTree[Vector[Property], Label]]
                                             )
  extends DecisionBranch[Vector[Property], Label, Property](v => v(index), branches)

/**
 * ID3 Algorithm. It creates a decision tree
 */
object ID3 {
  def buildTree[Property, Label](dataset: List[(Vector[Property], Label)]): DecisionTree[Vector[Property], Label] = {
    def indexWhereSplit(properties: Set[Int], dataset: List[(Vector[Property], Label)]) = {
      val entropies = for (i <- properties; // for each property
                           entropy = (for ((pv, subDataSet) <- splitOnProperty(dataset, i))
                           yield (subDataSet.length / dataset.length.toDouble) * subDataSet.entropy(_._2)
                           ).sum // and sum the entropy of all of them
      ) yield (entropy, i)

      // Take the property with the highest entropy gain
      entropies.reduceLeft((e1, e2) => if (e1._1 < e2._1) e1 else e2)._2
    }

    def splitOnProperty(dataset: List[(Vector[Property], Label)], propertyId: Int) =
      (
        for (
          propertyValue: Property <- (for ((data, label) <- dataset) yield data(propertyId)).toSet;
          subDataSet = for ((data, label) <- dataset if data(propertyId) == propertyValue) yield (data, label)
        ) yield (propertyValue, subDataSet)
      ) toMap

    def buildTreeRecursive(subset: List[(Vector[Property], Label)], properties: Set[Int]): DecisionTree[Vector[Property], Label] =
      subset.map(_._2).toSet.toList match {
        case List(oneLabel) => new DecisionLeaf[Vector[Property], Label](oneLabel)

        case _ => {
          val index = indexWhereSplit(properties, subset)
          new DecisionBranchVector[Label, Property](index,
            splitOnProperty(subset, index).mapValues(ds => buildTreeRecursive(ds, properties - index)))
        }
      }

    val features = Range(0, dataset.head._1.length, 1).toSet
    buildTreeRecursive(dataset, features)
  }
}


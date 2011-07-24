package org.scalarecog.knn

import org.scalarecog.Classifier

/**
 * k-NN algorithm. It classify a sample, given a dataset of known elements
 *
 * <p>
 * It needs: a dataset and a function to compute the distance between two items
 * </p>
 */
class Knn[Data, Label,
          Distance <% Ordered[Distance]]
(
  val k : Int,
  val dataset : List[(Data, Label)],
  val getDistance : (Data, Data) => Distance
)
extends Classifier[Data, Label]
{
  if(k < 1)
    throw new scala.IllegalArgumentException("k must be positive")

  def classify(sample : Data) : Label = {

    // Compute distances between sample and dataset items
    val distances = for(
                        (d, label) <- dataset
                    ) yield (d, label, getDistance(d, sample))

    // take first k items sorted by distance
    val firstk = distances sortBy ( d => d._3 ) take k

    // classification is given by the max number of occurrence of a label
    val classification = firstk groupBy (d => d._2) maxBy (g => g._2.length)

    classification._1
  }
}

object Knn {

  class KnnM[Data,Label] (data : List[(Data,Label)]) {
    def toKnn[Distance <% Ordered[Distance]](k : Int, distance : (Data, Data) => Distance) =
      new Knn[Data,Label,Distance](k, data, distance)
  }

  implicit def traversableToKnn[Data,Label](data : Traversable[(Data, Label)]) = new KnnM[Data,Label](data.toList)
}


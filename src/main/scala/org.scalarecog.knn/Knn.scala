package org.scalarecog.knn

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 17/07/11
 */

class Knn[TData, TClass,
          TDistance <% Ordered[TDistance]]
(
  val k : Int,
  val dataset : List[TData],
  val getClassOfData : TData => TClass,
  val getDistance : (TData, TData) => TDistance
)
{
  if(k < 1)
    throw new scala.IllegalArgumentException("k must be positive")

  def classify(data : TData) : TClass = {

    // Compute distances between tested data and dataset items
    val distances = for( d <- dataset ) yield (d, getDistance(d, data))

    // sort data by distance
    val sorted = distances sortBy ( d => d._2 )

    // take first k items
    val firstk = sorted take k

    val classes = firstk groupBy (d => getClassOfData(d._1))

    val classification = classes maxBy (g => g._2.length)

    return classification._1
  }
}

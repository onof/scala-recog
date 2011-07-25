package org.scalarecog.bayes

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import org.scalarecog.bayes.BayesClassifier._

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 25/07/11
 */

class BayesClassifierTest extends FlatSpec with ShouldMatchers {

  "BayesClassifier " should " classify a single labelled dataset" in {

    val dataSet = (List("Scala", "programming", "language"), "Programming") :: Nil

    val classifier = dataSet.asBayesTrainingSet

    (classifier classifyFuzzy List("Scala")) should equal ("Programming", 1.0)
    (classifier classifyFuzzy List("Scala", "programming")) should equal ("Programming", 1.0)
    (classifier classifyFuzzy List("Scala", "programming", "language")) should equal ("Programming", 1.0)
  }

  it should " classify easily a partitioned dataset" in {

    val dataSet = (List("Scala", "programming", "language", "Java", "JDK"), "Programming") ::
                  (List("Bach", "Beethoven", "Brahms", "Bruckner", "Scala"), "Music") :: Nil

    val classifier = dataSet asBayesTrainingSet

    (classifier classify List("Scala")) should equal ("Programming")
    (classifier classify List("Bach")) should equal ("Music")
    (classifier classify List("Scala", "Bach", "Bruckner")) should equal ("Music")
  }
}

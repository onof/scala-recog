package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 24/07/11
 */


trait Classifier[Data, Label] {

  def classify(data : Data) : Label

}

trait FuzzyClassifier[Data,Label] extends Classifier[Data,Label] {

  def classifyFuzzy(data : Data) : (Label, Double)

  def classify(data : Data) : Label = classifyFuzzy(data)._1
}

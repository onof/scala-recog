package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 24/07/11
 */
trait Classifier[Data, Label] {

  def classify(data : Data) : Label

}

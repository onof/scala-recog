package org.scalarecog.decisiontree

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 20/07/11
 */

trait DecisionTree[Data, Label] {
  def classify(something : Data) : Label
}

class DecisionLeaf[Data, Label](val label : Label)
  extends DecisionTree[Data, Label] {

  def classify(something : Data) : Label = label
}

class DecisionBranch[Data,Label,Property](
    val getProperty : Data => Property,
    val branches : Map[Property, DecisionTree[Data,Label]]
  )
  extends DecisionTree[Data, Label] {

  def classify(something : Data) : Label = branches.get(getProperty(something)) match {
    case Some(child) => child.classify(something)
    case None => throw new IndexOutOfBoundsException("unexpected value")
  }
}

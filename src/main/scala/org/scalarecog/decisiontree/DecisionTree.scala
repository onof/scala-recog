package org.scalarecog.decisiontree

import org.scalarecog.Classifier

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 20/07/11
 */
trait DecisionTree[Data, Label] extends Classifier[Data, Label]

class DecisionLeaf[Data, Label](val label : Label)
  extends DecisionTree[Data, Label] {

  def classify(something : Data) : Label = label

  override def toString = "Leaf(" + label + ")"
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

  override def toString = branches.mkString("Branch(", ",", ")")
}

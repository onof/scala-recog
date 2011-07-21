package org.scalarecog.decisiontree

import org.scalarecog.Entropy


/**
 * User: onofrio.panzarino@gmail.com
 * Date: 21/07/11
 */

class ID3[Property, Label] {
   type Data = Vector[Property]

   private val entropyCalc = new Entropy[(Data, Label), Label](g => g._2)

   private def indexWhereSplit(properties : Int, dataset : List[(Data, Label)]) = {
     val entropies = for(i <- 0 to properties; // for each property
                         entropy = (
                                    // for each distinct property
                                    for(feature <- (for ((data, label) <- dataset) yield data(i)).toSet;
                                        // split the dataset and compute the entropy
                                        subDataSet = for ((data, label) <- dataset if data(i) == feature) yield (data, label);
                                        prob = subDataSet.length / dataset.length.toDouble
                                        ) yield prob * entropyCalc(subDataSet)
                                    ).sum // and sum the entropy of all of them
                          ) yield (entropy, i)

     // Take the property with the highest entropy
     entropies.reduceLeft((e1, e2) => if(e1._1 > e2._1) e1 else e2)._2
   }

   def buildTree(dataset : List[(Data, Label)]) : DecisionTree[Data, Label] = {

     val features = dataset.head._1.length


     // TODO
     null
   }

}
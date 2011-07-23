package org.scalarecog.decisiontree

/**
 * Created by IntelliJ IDEA.
 * User: onofrio
 * Date: 23/07/11
 * Time: 22.33
 */

import org.scalatest._
import org.scalatest.matchers._


class DecisionTreeTest extends FlatSpec with ShouldMatchers {

  "DecisionLeaf " should " classify in a fixed class" in {
    val fixedClass = "fixed class"

    val dn = new DecisionLeaf[Int, String](fixedClass)

    dn.classify(32) should equal (fixedClass)
  }

  "DecisionBranch " should " breanch depending on two values" in {

    val branches = Map(1 -> new DecisionLeaf[Int, String]("One"),
                       2 -> new DecisionLeaf[Int, String]("Two"))
    val decBranch = new DecisionBranch[Int, String, Int](i => i, branches)

    decBranch classify 1 should equal ("One")
    decBranch classify 2 should equal ("Two")

    evaluating {
      decBranch classify 3
    } should produce [IndexOutOfBoundsException]
  }

}


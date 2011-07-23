package org.scalarecog.decisiontree

/**
 * Created by IntelliJ IDEA.
 * User: onofrio
 * Date: 23/07/11
 * Time: 22.34
 */

import org.scalatest._
import org.scalatest.matchers._

class ID3Test extends FlatSpec with ShouldMatchers {

  "ID3 " should " work with simple integer values" in {

    val dataset = (Vector(2), "Two") :: (Vector(1), "One") :: Nil

    val id3 = new ID3[Int, String]

    val tree = id3 buildTree dataset

    tree classify Vector(2) should equal("Two")
    tree classify Vector(1) should equal("One")

    evaluating(
       tree classify Vector(3)
    ) should produce [IndexOutOfBoundsException]
  }


}
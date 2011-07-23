package org.scalarecog.decisiontree

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 23/07/11
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

  it should " work with boolean vectors" in {
    val dataset  =   (Vector(true, false), "Cat") ::
                     // this is inferred:
                     //  (Vector(true, true),  "Cat") ::
                     (Vector(false, true), "Dog") ::
                     (Vector(false, false), "Dog") :: Nil

    val id3 = new ID3[Boolean,String]
    val tree = id3 buildTree dataset

    tree classify Vector(true, false) should equal ("Cat")
    tree classify Vector(true,  true) should equal ("Cat")
    tree classify Vector(false, true) should equal ("Dog")
    tree classify Vector(false,false) should equal ("Dog")
  }
}

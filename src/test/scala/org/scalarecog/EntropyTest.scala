package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 20/07/11
 */

import org.scalatest._
import org.scalatest.matchers._


class EntropyTest extends FlatSpec with ShouldMatchers {

  implicit val getProperty : Int => Int = i => i

  "Entropy of certainty " should " equal zero " in {

    val entropy1 = Entropy(1 :: Nil)
    entropy1 should equal (0.0)

    val entropy3 = Entropy(3 :: 3 :: 3 :: Nil)
    entropy3 should equal (0.0)
  }

  "Entropy " should " be symmetric" in {

    val entropy1 = Entropy(1 :: 2 :: 2 :: 3 :: Nil)
    val entropy2 = Entropy(1 :: 1 :: 2 :: 3 :: Nil)

    entropy1 should equal (entropy2)

  }

  it should " work with classes" in {

    def getProp(c : (String, Int)) = c._2

    val entropy = new Entropy[(String,Int), Int](getProp)
    val entropy1 = entropy(("first", 1)  :: ("second", 1) :: ("other", 2) :: ("other", 3) :: Nil)

    val entropy2 = Entropy(1 :: 1 :: 2 :: 3 :: Nil)

    entropy1 should equal (entropy2)

  }

}

package org.scalarecog

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 20/07/11
 */

import org.scalatest._
import org.scalatest.matchers._

import org.scalarecog.Entropy._

class EntropyTest extends FlatSpec with ShouldMatchers {

  "Entropy of certainty " should " equal zero " in {

    val entropy1 = (1 :: Nil).entropy
    entropy1 should equal (0.0)

    val entropy3 = (3 :: 3 :: 3 :: Nil).entropy
    entropy3 should equal (0.0)
  }

  "Entropy " should " be symmetric" in {

    val entropy1 = (1 :: 2 :: 2 :: 3 :: Nil).entropy
    val entropy2 = (1 :: 1 :: 2 :: 3 :: Nil).entropy

    entropy1 should equal (entropy2)

  }

  it should " work with classes" in {

    def getProp(c : (String, Int)) = c._2

    val entropy1 = (("first", 1)  :: ("second", 1) :: ("other", 2) :: ("other", 3) :: Nil).entropy(getProp)

    val entropy2 = (1 :: 1 :: 2 :: 3 :: Nil).entropy

    entropy1 should equal (entropy2)

  }

}

package org.scalarecog.knn

/**
 * User: onofrio.panzarino@gmail.com
 * Date: 17/07/11
 */

import org.scalatest._
import org.scalatest.matchers._

import scala.math._

class KnnTest extends FlatSpec with ShouldMatchers {

  "2nn" should " classify correctly with a sample near two well-known labelled items" in {
    val alg = new Knn(2,
          List((3, "ok"), (18, "ok"), (21, "high"), (64, "high")),
          (i1: Int, i2 : Int) => abs(i1 - i2))

    val classified = alg.classify(4)

    classified should equal ("ok")
  }

  it should " throw if k is negative " in {
    evaluating {
       val alg = new Knn(-2,
          List((3, "ok"), (18, "ok"), (21, "high"), (64, "high")),
          (i1: Int, i2 : Int) => abs(i1 - i2))
    } should produce [IllegalArgumentException]
  }
}

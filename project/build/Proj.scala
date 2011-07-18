import sbt._

class MyProject(info: ProjectInfo) extends DefaultProject(info) {

    // Dependencies

  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

}


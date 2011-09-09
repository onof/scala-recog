import sbt._

class MyProject(info: ProjectInfo) extends DefaultProject(info) {

  // Dependencies
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

  //
  val scalaNLPRepo = "ScalaNLP" at "http://repo.scalanlp.org/repo/"
  val ondexRepo = "Ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public/"

  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val scalala = "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT"


  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Sonatype Nexus Repository Manager" at "http://nexus.scala-tools.org/content/repositories/snapshots"

  Credentials(Path.userHome / ".ivy2" / "scalatools", log)
}

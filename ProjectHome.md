**scala-recog** is a library written in [Scala](http://www.scala-lang.org/) to provide general implementations of machine learning algorithms.

# Features #

## Classifiers ##
  * k-NN
  * ID3: see [Decision Tree example](http://onof80en.blogspot.com/2011/07/decision-trees-with-scala-recog-machine.html)
  * Bayes: see [Bayes example](BayesExample.md)
  * Logistic Regression: see [Logistic Regression Traits ](LogisticRegressionTraits.md)

## Utilities ##
  * Shannon Entropy (discrete)

# Getting started #
## sbt ##
Builds are hosted on [scala-tools](http://scala-tools.org). To include it, add this to your project (sbt 0.7.7):

```
val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val scalaNLPRepo = "ScalaNLP" at "http://repo.scalanlp.org/repo/"
  val ondexRepo = "Ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public/"

val scalarecog = "com.googlecode.scala-recog" %% "scala-recog" % "0.2.3-SNAPSHOT"
```

## Download ##
If you don't use SBT, you can download the package from the Downloads section and include it.
You will need also to import [Scalala](https://github.com/scalala/Scalala/wiki/QuickStart) in your project.

# Building scala-recog #
scala-recog is currently built with  sbt 0.7.7 (you can download and install it from the [former hosting site](http://code.google.com/p/simple-build-tool/downloads/list)).
  * checkout the code
  * go to the folder and run sbt
  * launch update action to get the reference
  * more details [here](http://code.google.com/p/simple-build-tool/wiki/RunningSbt)
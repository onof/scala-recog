# Classifying documents #

To classify documents you can use the [Bayes' theorem](http://en.wikipedia.org/wiki/Bayes'_theorem).
You need a training set of classified documents, and a vocabulary of keywords you can extract from them. For instance, you have some documents about **programming** and some about **music**. The extracted keywords are:

```
val dataSet = (List("Scala","programming","language","Java","JDK"), "Programming") ::
              (List("Bach","Beethoven","Scala","Theatre"), "Music") :: 
              (List("Bruckner", "Brahms"), "Music") ::
              Nil
```

## Using the dataset to train the classifier ##

Use implicit conversion in `BayesClassifier` to train the classifier:
```
import org.scalarecog.bayes.BayesClassifier._

val classifier = dataSet asBayesTrainingSet
```

## Classify a new document ##
```
val topic = classifier classify List("Scala", "Bach", "Verdi")
```
Now topic should be "Music"
# Flexibility with logistic regression #

Logistic regression works with vectors and scala-recog uses [scalala](http://github.com/scalala/Scalala) as linear algebra library. So you need to convert your training set in scalala's [VectorRows](http://github.com/scalala/Scalala/wiki/QuickStart).
Maybe scala-recog will provide some conversion utilities, but now you need to create your training set and class labels as vectors (or matrices).

## Using default settings ##
By default logistic regression uses a Stochastic Gradient Ascent training algorithm with some fixes to reduce error.
For instance, with this training set: (don't use it, it's just a sequence of random vectors)

```
   import scala.math._;
   import scalala.tensor.::
   import scalala.tensor.mutable._;
   import scalala.tensor.dense._;

   val ts = for ( i <- 0 until 300 )
          yield (DenseVector.tabulate(2)(i => random*10.0).t, random > 0.5)
```

then you can train the classifier this way:

```
   import org.scalarecog.logisticregression.SigmoidClassifier._
   val classifier = ts.train

   // and now we can classify a new item
   classifier classify Vector(5, 2)
```

## Using stackable traits to get more flexibility ##
If you want to hack the training algorithm to change number of iterations or some other features you can use the power of [stackable modifications](http://www.artima.com/scalazine/articles/stackable_trait_pattern.html) and class composition.

Here are some examples

### To change the max number of iterations ###
Just override the value:
```
   val trainer1 = new SigmoidTrainer with ImprovedStochasticGradientAscent with FixedIterations {
      val trainingSet = ts
      override val numIterations = 1000
    }
```

### Disabling improvements used by default ###
If you want the naive stochastic gradient ascent algorithm with custom overrides:
```
   val trainer2 = new SigmoidTrainer with StochasticGradientAscent with FixedIterations {
      val trainingSet = ts

      override def alpha(i : Int) : Double = 2/(1.0+i )+0.01
    }
```

If you want a gradient ascent algorithm, not the stochastic one (in this case the training set should be a matrix):
```
   val trainer3 = new SigmoidTrainer with GradientAscent with FixedIterations {
      val trainingSet = DenseMatrix.zeros[Double](50, 3)
      val labels = DenseVectorCol.zeros[Double](50)
      override val numIterations : Int = 150
    }
```

### Implement your own stop condition ###
You can use the stochastic gradient ascent algorithm with a stop condition other than the fixed iterations

```
   trait MyStopCondition extends Trainer[Double] {
      lazy val weights = {
         var w = startWeights
         while(/* more code */)
          w = improveWeights(w)
         w
      }
    }

    val trainer4 = new SigmoidTrainer with StochasticGradientAscent with MyStopCondition {
      val trainingSet = ts
    }

```


### Implement your own optimization algorithm ###
```
   val trainer5 = new SigmoidTrainer with FixedIterations {
      val startWeights = DenseVectorCol.ones[Double](32)
      val trainingSet = ts
      
      def improveWeights(weights : VectorCol[Double]) = /* ... more code ... */

    }
```
# Predictive Monitoring of business processes

This repository contains the code from my master's thesis "Feature extraction from event logs for predictive monitoring of business processes", which is included as `thesis_fborchert.pdf`. 

There are 2 subprojects:

* `feature_extraction` contains the implementation of tree-based trace profiles and general classes for transforming event log cases to [WEKA](http://www.cs.waikato.ac.nz/ml/weka/) instances using a trace profile
* `process_prediction` contains the experimental setup with different trace profiles in a binary classification and an outlier detection setting

## How to run the experiments for your own logs 

### Requirements:

* [Scala 2.11](http://www.scala-lang.org/)
* [sbt](http://www.scala-sbt.org/)

### Configuration

You need to edit the file `process_prediction/config.properties` to specifiy the parameters for your data.

* `title` - the title of your experiments for the result files, e.g. `title = My Experiment` 
* `type` - the type of algorithm you want to use, valid values are `type=randomforest` and `type=outlier`, but extensions should be straightforward to implement
* `training`- the folder containing the training data in [XES](http://www.xes-standard.org/) format
* `test` - the folder containing the hold out test data in XES format
* `ingore-attributes` - a comma-separated list of case attributes (by regular expressions) that should be ignored, e.g. `ignore-attributes=year,applicant,payment_granted[^0].*,payment_actual[^0].*,cutting[^0].*`
* `prefix` - the full qualified class name of a Scala class that maps traces to prefixes, e.g. `prefix=input.ExamplePrefix` (see the `process_prediction\src\main\scala\input` folder for an example)
* `output` - the full qualified class name of a Scala class that maps traces to labels. If `type=outlier`, this label is used for evaluation (see the `process_prediction\src\main\scala\input` folder for an example)
* `IM-noise` - the noise threshhold for the Inductive Miner used to construct tree-based trace profiles, e.g. `IM-noise=0.8`. The parameter is optional and chosen via cross-validation if not supplied
* `nu`- the nu parameter of the OC-SVM used for outlier detection, e.g. `nu=0.05`. This parameter is optional and set to 0.05 if absent and `type=outlier` has been set 

### Run it

Go to the root folder of this project and run the following commands:

* `sbt` (opens the sbt console)
* `compile`
* `project process_prediction`
* `run`

When you properly configured the experimental setup, this should run a series of experiments and place results in a folder named `results/<title>`

## Supplying different trace profiles

You can change the implementation of the `getConfigs()` methods in:

* `process_prediction/src/main/scala/de/huberlin/informatik/promi/experiments/BinaryClassification.scala`
* `process_prediction/src/main/scala/de/huberlin/informatik/promi/experiments/outlier/Outlier.scala`

to modify the experimental setup, edit the trace profiles used or supply your own.

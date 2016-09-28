# Predictive Monitoring of business processes

This repository contains the code from my master's thesis "Feature extraction from event logs for predictive monitoring of business processes", which is included as `thesis_fborchert.pdf`. 

The project contains 2 subprojects:

* `feature_extraction` contains the implementation of tree-based trace profiles and general classes for transforming trace profiles to WEKA instances
* `process_prediction` contains the experimental setup with different trace profiles in a binary classification and an outlier detection setting

## How to run the experiments for your own logs 

### Requirements:

* Scala 2.11 (http://www.scala-lang.org/)
* sbt (http://www.scala-sbt.org/)

### Configuration

You need to edit the file `process_prediction/config.properties` to specifiy the parameters for your data.

* `title` - the title of your experiments for the result files, e.g. `title = My Experiment` 
* `type` - the type of algorithm you want to use, valid values are `type=randomforest` and `type=outlier`, but extensions should be straightforward to implement
* `training`- the folder containing the training data in XES format
* `test` - the folder containing the hold out test data in XES format
* `ingore-attributes` - a comma-separated list of case attributes (by regular expressions) that should be ignored, e.g. `ignore-attributes=year,applicant,payment_granted[^0].*,payment_actual[^0].*,cutting[^0].*`
* `prefix` - the full qualified class name of a Scala class that maps traces to prefixes, e.g. `prefix=input.ExamplePrefix` (look in src folder for an implementation)
* `output` - the full qualified class name of a Scala class that maps traces to labels. If `type=outlier`, this label is used for evaluation
* `IM-noise` - the noise threshhold for the Inductive Miner used to construct tree-based trace profiles, e.g. `IM-noise=0.8`. The parameter is optional and chosen via cross-validation if not supplied

### Run it

Go to the root folder of this project and run the following commands:

* `sbt` (opens the sbt console)
* `compile`
* `project process_prediction`
* `run`

When you properly configured the experimental setup, this should run a series of experiments and place results in a folder named `results/title`
package de.huberlin.informatik.promi.experiments.outlier

import de.huberlin.informatik.promi.F1
import de.huberlin.informatik.promi.GridSearch
import de.huberlin.informatik.promi.PositiveIgnorer
import de.huberlin.informatik.promi.LibSVM

trait OCSVMRBF extends Outlier {

  val classyName = "OC-SVM RBF " + nu
  
  def gridSearch =
    Some(GridSearch(
      "gamma" -> gammaVals,
      (gamma, config) =>
        config.copy(
          classifier = Some(
            wrapper(LibSVM(
              gamma = gamma,
              options = "-Z" :: "-W" :: "1" :: "-S" :: "2" :: "-N" :: nu.toString() :: Nil))))).
      optimizeFor(F1))

}
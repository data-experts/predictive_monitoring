package de.huberlin.informatik.promi.experiments

import de.huberlin.informatik.promi.util.Splitter._

trait HasSplit {
  def split : Split
  def files : String
}

trait TrainTestSplit extends HasSplit {
  def split = TrainingTest(files + "training", files + "test")
}

trait ByYearSplit  extends HasSplit {
  def split  = AsInFiles(4, 0, 1)
}
  